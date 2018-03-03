{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- This test tests the following the high-level subscription consumption.

module Network.Nakadi.Subscriptions.Processing.Test where

import           ClassyPrelude

import           Control.Concurrent.Async    (link)
import           Control.Lens
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Maybe                  (fromJust)
import qualified Data.Vector                 as Vector
import           Network.Nakadi
import qualified Network.Nakadi.Lenses       as L
import           Network.Nakadi.Tests.Common
import           System.Random
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

data MyException = MyException deriving (Show, Typeable)

instance Exception MyException

testIORefException :: Assertion
testIORefException = do
  let nIterations = 500
  forM_ [1..nIterations] $ \ idx -> do
    n :: Int <- randomRIO (10000, 20000)
    putStrLn $ "Iteration: " ++ tshow idx ++ " (rand n = " ++ tshow n ++ ")"
    sharedRef <- newIORef 0
    count n sharedRef `catch` \ MyException -> pure ()
    shared <- readIORef sharedRef
    n @=? shared

  where count n ref = do
          forM_ [1..n] $ \ _ -> do
            modifyIORef ref (+ 1)
            current <- readIORef ref
            when (current == n) $ throwM MyException

testSubscriptionHighLevelProcessing :: Config App -> Assertion
testSubscriptionHighLevelProcessing conf = runApp $ do
  logger <- askLoggerIO
  let logFunc src lev str = liftIO $ logger defaultLoc src lev str
  runNakadiT (conf & setLogFunc logFunc) $ do
    counter <- newIORef 0
    events <- sequence $
      map genMyDataChangeEventIdx [1..nEvents] :: NakadiT App App [DataChangeEvent Foo]
    publishAndConsume events counter `catchAny` \ exn -> do
      putStrLn $ "Caught exception: " ++ tshow exn
    eventsRead <- readIORef counter
    putStrLn $ "Counter content: " <> tshow eventsRead
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
              putStrLn $ "Consumed batch. Cursor: " ++ tshow (batch^.L.cursor.L.offset) ++ "; numbers of events: " ++ tshow (length eventsReceived) ++ "; first event = " ++ tshow (eventsReceived Vector.!? 0)
              modifyIORef counter (+ (length eventsReceived))
              eventsRead <- readIORef counter
              when (n == eventsRead) $ do
                putStrLn $
                  "Throwing ConsumptionDone exception. Counter content is " <> tshow eventsRead
                throwM ConsumptionDone

        consumeParameters = defaultConsumeParameters
                            & setBatchFlushTimeout 1
                            & setMaxUncommittedEvents 5000
                            & setBatchLimit 10
