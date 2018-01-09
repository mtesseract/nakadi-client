{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Examples.Echo.Test (testEcho) where

import           ClassyPrelude
import           Conduit
import           Control.Concurrent.Async.Lifted          (wait)
import           Control.Lens
import qualified Network.Nakadi                    as Nakadi
import           Network.Nakadi.Examples.Echo.Echo
import qualified Network.Nakadi.Lenses             as L
import qualified Data.Vector as Vector
import           Test.Tasty.HUnit
import           Network.Nakadi.Tests.Common

-- Example program which consumes the events for the event type
-- "test-event" and republishes them unchanged under the event type
-- "test-event-copy".

genEvent :: MonadIO m => m (Nakadi.DataChangeEvent Foo)
genEvent = do
  now <- liftIO getCurrentTime
  eid <- Nakadi.EventId <$> genRandomUUID
  let event = Nakadi.DataChangeEvent
        { Nakadi._payload = Foo "Hello!"
        , Nakadi._metadata = Nakadi.Metadata
                             { Nakadi._eid = eid
                             , Nakadi._occurredAt = Nakadi.Timestamp now
                             , Nakadi._parentEids = Nothing
                             , Nakadi._partition = Nothing
                             }
        , Nakadi._dataType = "test.FOO"
        , Nakadi._dataOp = Nakadi.DataOpUpdate
        }
  pure event

genEvents :: MonadIO m => m (Vector (Nakadi.DataChangeEvent Foo))
genEvents =
  Vector.fromList <$> sequence (replicate 10 genEvent)

publishEvents :: Nakadi.MonadNakadi IO m
              => Vector (Nakadi.DataChangeEvent Foo)
              -> Nakadi.EventTypeName -> m ()
publishEvents events eventName = do
  Nakadi.eventPublish eventName Nothing (Vector.toList events)
  
consumerMain :: (Nakadi.MonadNakadi IO m, MonadBaseControl IO m)
             => Nakadi.EventTypeName
             -> Int
             -> m (Vector Foo)
consumerMain eventName maxSize = runResourceT $ do
  let consumeParameters = Nakadi.defaultConsumeParameters
                          & L.batchFlushTimeout .~ Just 1
                          & L.streamLimit .~ Just (fromIntegral maxSize)
  source <- Nakadi.eventSource (Just consumeParameters) eventName Nothing
  events <- runConduit $
            source
            .| concatMapC (view L.events)
            .| concatC
            .| mapC (id :: Nakadi.EventEnriched Foo -> Nakadi.EventEnriched Foo)
            .| mapC (view L.payload)
            .| sinkVector
  pure events

testEcho :: Nakadi.Config IO -> Assertion
testEcho config = Nakadi.runNakadiT config $ do
  recreateEvent myEventTypeName myEventType
  recreateEvent myEventTypeNameCopy myEventTypeCopy
  events <- genEvents
  let eventsPayloads = map (view L.payload) events
  withAsync (runEcho myEventTypeName myEventTypeNameCopy) $ \ _echoHandle ->
    withAsync (consumerMain myEventTypeNameCopy (length events)) $ \ consumerHandle -> do
    threadDelay (10^6) -- Give it some time to connect
    publishEvents events myEventTypeName
    eventsConsumed <- wait consumerHandle
    liftIO $ eventsPayloads @=? eventsConsumed

  where myEventTypeNameCopy = Nakadi.EventTypeName "test.FOO-copy"
        myEventTypeCopy = myEventType & L.name .~ myEventTypeNameCopy
