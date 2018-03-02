{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- This test tests the following the high-level subscription consumption.

module Network.Nakadi.Subscriptions.Processing.Test where

import           ClassyPrelude

import           Control.Concurrent.Async    (link)
import           Control.Lens
import           Data.Aeson
import           Data.Conduit
import           Data.Maybe                  (fromJust)
import           Network.Nakadi
import qualified Network.Nakadi.Lenses       as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testSubscriptionsProcessing :: Config App -> TestTree
testSubscriptionsProcessing confTemplate =
  let mkConf commitStrategy = confTemplate
                              & setCommitStrategy commitStrategy
  in testGroup "Processing"
     [ testCase "SubscriptionProcessing/async/TimeBuffer" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync (CommitTimeBuffer 200)))
     , testCase "SubscriptionProcessing/sync" $
       testSubscriptionHighLevelProcessing (mkConf CommitSync)
     , testCase "SubscriptionProcessing/async/NoBuffer" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitNoBuffer))
     , testCase "SubscriptionProcessing/async/SmartBuffer" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitSmartBuffer))
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

  where before :: MonadNakadi App m => m SubscriptionId
        before = do
          recreateEvent myEventTypeName myEventType
          subscription <- subscriptionCreate Subscription
            { _id = Nothing
            , _owningApplication = "test-suite"
            , _eventTypes = [myEventTypeName]
            , _consumerGroup = Nothing -- ??
            , _createdAt = Nothing
            , _readFrom = Just SubscriptionPositionBegin
            , _initialCursors = Nothing
            }
          pure . fromJust $ subscription^.L.id

        after :: MonadNakadi App m => SubscriptionId -> m ()
        after subscriptionId = do
          subscriptionDelete subscriptionId
          eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())

        nEvents :: Int
        nEvents = 10000

        publishAndConsume :: (ToJSON a, FromJSON a)
                          => [DataChangeEvent a]
                          -> IORef Int
                          -> NakadiT App App ()
        publishAndConsume events counter =
          bracket before after $ \ subscriptionId -> do
          let n = length events
          publisherHandle <- async $ do
            delayedPublish Nothing events
          liftIO $ link publisherHandle
          subscriptionProcess (Just consumeParameters) subscriptionId $
            \ (batch :: SubscriptionEventStreamBatch (DataChangeEvent Foo)) -> do
              let eventsReceived = fromMaybe mempty (batch^.L.events)
              modifyIORef counter (+ (length eventsReceived))
              eventsRead <- readIORef counter
              when (n == eventsRead) $ throwM ConsumptionDone

        consumeParameters = defaultConsumeParameters
                            & setBatchFlushTimeout 1
                            & setMaxUncommittedEvents 5000
                            & setBatchLimit 10

testSubscriptionConduitProcessing :: Config App -> Assertion
testSubscriptionConduitProcessing conf = runApp . runNakadiT conf $ do
  counter <- newIORef 0
  events <- sequence $
    replicate nEvents genMyDataChangeEvent :: NakadiT App App [DataChangeEvent Foo]
  publishAndConsume events counter `catch` \ (_exn :: ConsumptionDone) -> pure ()
  eventsRead <- readIORef counter
  liftIO $ nEvents @=? eventsRead

  where before :: MonadNakadi App m => m SubscriptionId
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

        after :: MonadNakadi App m => SubscriptionId -> m ()
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
                            & setMaxUncommittedEvents 5000
                            & setBatchLimit 10
