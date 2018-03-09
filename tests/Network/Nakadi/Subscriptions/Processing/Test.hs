{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- This test tests the high-level subscription consumption.

module Network.Nakadi.Subscriptions.Processing.Test where

import           ClassyPrelude

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Maybe                   (fromJust)
import           Network.Nakadi
import qualified Network.Nakadi.Lenses        as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

testSubscriptionsProcessing :: Config App -> TestTree
testSubscriptionsProcessing confTemplate =
  let mkConf commitStrategy nWorkers = confTemplate
                                       & setCommitStrategy commitStrategy
                                       & setWorkerThreads nWorkers
  in testGroup "Processing"
     [ testCase "SubscriptionProcessing/async/TimeBuffer/singleWorker" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync (CommitTimeBuffer 200)) 1)
     , testCase "SubscriptionProcessing/sync/singleWorker" $
       testSubscriptionHighLevelProcessing (mkConf CommitSync 1)
     , testCase "SubscriptionProcessing/async/NoBuffer/singleWorker" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitNoBuffer) 1)
     , testCase "SubscriptionProcessing/async/SmartBuffer/singleWorker" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitSmartBuffer) 1)
     , testCase "SubscriptionProcessing/async/TimeBuffer/concurrentWorkers" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync (CommitTimeBuffer 200)) 8)
     , testCase "SubscriptionProcessing/sync/concurrentWorkers" $
       testSubscriptionHighLevelProcessing (mkConf CommitSync 8)
     , testCase "SubscriptionProcessing/async/NoBuffer/concurrentWorkers" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitNoBuffer) 8)
     , testCase "SubscriptionProcessing/async/SmartBuffer/concurrentWorkers" $
       testSubscriptionHighLevelProcessing (mkConf (CommitAsync CommitSmartBuffer) 8)
     ]

data ConsumptionDone = ConsumptionDone deriving (Show, Typeable)

instance Exception ConsumptionDone

testSubscriptionHighLevelProcessing :: Config App -> Assertion
testSubscriptionHighLevelProcessing conf = runApp $ do
  logger <- askLoggerIO
  let logFunc src lev str = liftIO $ logger defaultLoc src lev str
  runNakadiT (conf & setLogFunc logFunc) $ do
    counter <- newIORef 0
    events <- sequence $
      map genMyDataChangeEventIdx [1..nEvents] :: NakadiT App App [DataChangeEvent Foo]
    publishAndConsume events counter `catch` \ (_exn :: ConsumptionDone) -> pure ()
    eventsRead <- readIORef counter
    liftIO $ nEvents @=? eventsRead

  where before :: (MonadUnliftIO m, MonadNakadi App m) => m SubscriptionId
        before = do
          recreateEvent myEventType
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

        after :: (MonadUnliftIO m, MonadNakadi App m) => SubscriptionId -> m ()
        after subscriptionId = do
          subscriptionDelete subscriptionId
          eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())

        nEvents :: Int
        nEvents = 5000

        publishAndConsume :: (ToJSON a, FromJSON a, Show a)
                          => [DataChangeEvent a]
                          -> IORef Int
                          -> NakadiT App App ()
        publishAndConsume events counter =
          bracket before after $ \ subscriptionId -> do
          let n = length events
          publisherHandle <- async $ do
            delayedPublish Nothing events
          liftIO $ linkAsync publisherHandle
          forever $ do
            runResourceT $
              subscriptionProcess (Just consumeParameters) subscriptionId $
              \ (batch :: SubscriptionEventStreamBatch (DataChangeEvent Foo)) -> do
                let eventsReceived = fromMaybe mempty (batch^.L.events)
                modifyIORef counter (+ (length eventsReceived))
                eventsRead <- readIORef counter
                when (n <= eventsRead) $ throwIO ConsumptionDone
            putStrLn $ "Subscription Processing terminated, will restart."

        consumeParameters = defaultConsumeParameters
                            & setBatchFlushTimeout 1
                            & setMaxUncommittedEvents 5000
                            & setBatchLimit 10
