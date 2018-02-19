{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- This test tests the following the high-level subscription consumption.

module Network.Nakadi.Subscriptions.Processing.Test where

import           ClassyPrelude

import           Control.Lens
import           Data.Aeson
import           Data.Conduit
import           Data.Maybe                  (fromJust)
import           Network.Nakadi
import qualified Network.Nakadi.Lenses       as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import Control.Monad.Catch (throwM)

testSubscriptionsProcessing :: Config App -> TestTree
testSubscriptionsProcessing conf = testGroup "Processing"
  [ testCase "SubscriptionProcessing" (testSubscriptionHighLevelProcessing conf)
  ]

data ConsumptionDone = ConsumptionDone deriving (Show, Typeable)


instance Exception ConsumptionDone

testSubscriptionHighLevelProcessing :: Config App -> Assertion
testSubscriptionHighLevelProcessing conf = runApp . runNakadiT conf $ do
  counter <- newIORef 0
  events <- sequence $
    replicate nEvents genMyDataChangeEvent :: NakadiT App App [DataChangeEvent Foo]
  publishAndConsume events counter `catch` \ (_exn :: ConsumptionDone) -> pure ()
  eventsRead <- readIORef counter
  liftIO $ nEvents @=? eventsRead

  where before :: (MonadUnliftIO m, MonadNakadi App m)
               => m SubscriptionId
        before = do
          recreateEvent myEventTypeName myEventType
          subscription <- subscriptionCreate Subscription
            { _id = Nothing
            , _owningApplication = "test-suite"
            , _eventTypes = [myEventTypeName]
            , _consumerGroup = Nothing -- ??
            , _createdAt = Nothing
            , _readFrom = Just SubscriptionPositionEnd
            , _initialCursors = Nothing
            }
          pure . fromJust $ subscription^.L.id

        after :: (MonadUnliftIO m, MonadNakadi App m)
              => SubscriptionId -> m ()
        after subscriptionId = do
          subscriptionDelete subscriptionId
          eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())

        nEvents :: Int
        nEvents = 100

        publishAndConsume :: (ToJSON a, FromJSON a)
                          => [DataChangeEvent a]
                          -> IORef Int
                          -> NakadiT App App ()
        publishAndConsume events counter =
          bracket before after $ \ subscriptionId -> do
          let n = length events
          void . async $ delayedPublish Nothing events
          subscriptionProcess (Just consumeParameters) subscriptionId $
            \ (batch :: SubscriptionEventStreamBatch (DataChangeEvent Foo)) -> do
              -- Make sure that we can use Nakadi from within the high
              -- level processing callback:
              void $ registryPartitionStrategies
              let eventsReceived = fromMaybe mempty (batch^.L.events)
              modifyIORef counter (+ (length eventsReceived))
              eventsRead <- readIORef counter
              when (n == eventsRead) $ throwM ConsumptionDone

        consumeParameters = defaultConsumeParameters
                            & setBatchFlushTimeout 1
                            & setBatchLimit 1

testSubscriptionConduitProcessing :: Config App -> Assertion
testSubscriptionConduitProcessing conf = runApp . runNakadiT conf $ do
  counter <- newIORef 0
  events <- sequence $
    replicate nEvents genMyDataChangeEvent :: NakadiT App App [DataChangeEvent Foo]
  publishAndConsume events counter `catch` \ (_exn :: ConsumptionDone) -> pure ()
  eventsRead <- readIORef counter
  liftIO $ nEvents @=? eventsRead

  where before :: (MonadUnliftIO m, MonadNakadi App m)
               => m SubscriptionId
        before = do
          recreateEvent myEventTypeName myEventType
          subscription <- subscriptionCreate Subscription
            { _id = Nothing
            , _owningApplication = "test-suite"
            , _eventTypes = [myEventTypeName]
            , _consumerGroup = Nothing -- ??
            , _createdAt = Nothing
            , _readFrom = Just SubscriptionPositionEnd
            , _initialCursors = Nothing
            }
          pure . fromJust $ subscription^.L.id

        after :: (MonadUnliftIO m, MonadNakadi App m)
              => SubscriptionId -> m ()
        after subscriptionId = do
          subscriptionDelete subscriptionId
          eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())

        nEvents :: Int
        nEvents = 100

        publishAndConsume :: (ToJSON a, FromJSON a)
                          => [DataChangeEvent a]
                          -> IORef Int
                          -> NakadiT App App ()
        publishAndConsume events counter =
          bracket before after $ \ subscriptionId -> do
          let n = length events
          void . async $ delayedPublish Nothing events
          subscriptionProcessConduit (Just consumeParameters) subscriptionId $ do
            awaitForever $ \ (batch :: SubscriptionEventStreamBatch (DataChangeEvent Foo)) -> do
              -- Make sure that we can use Nakadi via lifting directly
              -- from within the Conduit.
              void $ lift registryPartitionStrategies
              let eventsReceived = fromMaybe mempty (batch^.L.events)
              modifyIORef counter (+ (length eventsReceived))
              eventsRead <- readIORef counter
              when (n == eventsRead) $ throwM ConsumptionDone
              yield batch

        consumeParameters = defaultConsumeParameters
                            & setBatchFlushTimeout 1
                            & setBatchLimit 1
