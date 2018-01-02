{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.EventTypes.Test where

import           ClassyPrelude

import           Conduit
import           Control.Concurrent.Async                      (link)
import           Control.Lens
import           Data.Function                                 ((&))
import           Network.Nakadi
import           Network.Nakadi.EventTypes.CursorsLag.Test
import           Network.Nakadi.EventTypes.ShiftedCursors.Test
import qualified Network.Nakadi.Lenses                         as L
import           Network.Nakadi.Tests.Common
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

testEventTypes :: Config' App -> TestTree
testEventTypes conf = testGroup "EventTypes"
  [ testCase "EventTypesPrepare" (testEventTypesPrepare conf)
  , testCase "EventTypesGet" (testEventTypesGet conf)
  , testCase "EventTypesDeleteCreateAndGet" (testEventTypesDeleteCreateGet conf)
  , testCase "EventTypePartitionsGet" (testEventTypePartitionsGet conf)
  , testCase "EventTypeCursorDistances0" (testEventTypeCursorDistances0 conf)
  , testCase "EventTypeCursorDistances10" (testEventTypeCursorDistances10 conf)
  , testCase "EventTypePublishData" (testEventTypePublishData conf)
  , testCase "EventTypeParseFlowId" (testEventTypeParseFlowId conf)
  , testCase "EventTypeDeserializationFailure" (testEventTypeDeserializationFailure conf)
  , testEventTypesShiftedCursors conf
  , testEventTypesCursorsLag conf
  ]

testEventTypesPrepare :: Config' App -> Assertion
testEventTypesPrepare conf = runApp . runNakadiT conf $ do
  subscriptions <- subscriptionsList conf Nothing Nothing
  let subscriptionIds = catMaybes . map (view L.id) $ subscriptions
  forM_ subscriptionIds (subscriptionDelete conf)

testEventTypesGet :: Config' App -> Assertion
testEventTypesGet conf = runApp . runNakadiT conf $
  void $ eventTypesList conf

testEventTypesDeleteCreateGet :: Config' App -> Assertion
testEventTypesDeleteCreateGet conf = runApp . runNakadiT conf $ do
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  myEventTypes <- filterMyEvent <$> eventTypesListR
  liftIO $ length myEventTypes @=? 1
  eventTypeDeleteR myEventTypeName
  myEventTypes' <- filterMyEvent <$> eventTypesListR
  liftIO $ length myEventTypes' @=? 0

  where filterMyEvent = filter ((myEventTypeName ==) . (view L.name))

testEventTypePartitionsGet :: Config' App -> Assertion
testEventTypePartitionsGet conf = runApp . runNakadiT conf $ do
  eventTypeDelete conf myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate conf myEventType
  void $ eventTypePartitions conf myEventTypeName

testEventTypeCursorDistances0 :: Config' App -> Assertion
testEventTypeCursorDistances0 conf = runApp . runNakadiT conf $ do
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  partitions <- eventTypePartitionsR myEventTypeName
  let cursors = map extractCursor partitions
  forM_ cursors $ \cursor -> do
    distance <- cursorDistanceR myEventTypeName cursor cursor
    liftIO $ distance @=? 0

testEventTypeCursorDistances10 :: Config' App -> Assertion
testEventTypeCursorDistances10 conf = runApp . runNakadiT conf $ do
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  partitions <- eventTypePartitions conf myEventTypeName
  let cursors = map extractCursor partitions

  forM_ [1..10] $ \_ -> do
    now <- liftIO getCurrentTime
    eid <- EventId <$> genRandomUUID
    eventPublishR myEventTypeName Nothing [myDataChangeEvent eid now]

  cursorPairs <- forM cursors $ \cursor@Cursor { .. } -> do
    part <- eventTypePartitionR myEventTypeName _partition
    let cursor' = extractCursor part
    return (cursor, cursor')

  distances <- forM cursorPairs $ \(c, c') -> do
    cursorDistanceR myEventTypeName c c'

  let totalDistances = sum distances
  liftIO $ totalDistances @=? 10

consumeParametersSingle :: ConsumeParameters
consumeParametersSingle = defaultConsumeParameters
                          & setBatchLimit 1
                          & setBatchFlushTimeout 1

testEventTypePublishData :: Config' App -> Assertion
testEventTypePublishData conf = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = Metadata { _eid = eid
                                                     , _occurredAt = Timestamp now
                                                     , _parentEids = Nothing
                                                     , _partition = Nothing
                                                     }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
  withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch Foo) <- runResourceT $ do
      source <- eventSource conf (Just consumeParametersSingle) myEventTypeName Nothing
      runConduit $ source .| headC
    liftIO $ isJust eventConsumed @=? True

testEventTypeParseFlowId :: Config' App -> Assertion
testEventTypeParseFlowId conf = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = Metadata { _eid = eid
                                                     , _occurredAt = Timestamp now
                                                     , _parentEids = Nothing
                                                     , _partition = Nothing
                                                     }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
      expectedFlowId = Just $ FlowId "12345"
  withAsync (delayedPublish expectedFlowId [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch Foo) <- runResourceT $ do
      source <- eventSourceR (Just consumeParametersSingle) myEventTypeName Nothing
      runConduit $ source .| headC
    liftIO $ isJust eventConsumed @=? True
    let events = eventConsumed >>= (\batch -> batch^.L.events)
    liftIO $ isJust events @=? True

    liftIO $ case events of
      Nothing -> assertFailure "Received no events"
      Just v -> case toList v of
        [EventEnriched _ x] ->
          x^.L.flowId @=? expectedFlowId
        _ -> assertFailure "Received not a singleton event list"

testEventTypeDeserializationFailure :: Config' App -> Assertion
testEventTypeDeserializationFailure conf' = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDeleteR myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreateR myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = Metadata { _eid = eid
                                                     , _occurredAt = Timestamp now
                                                     , _parentEids = Nothing
                                                     , _partition = Nothing
                                                     }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
  withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch WrongFoo) <- runResourceT $ do
      source <- eventSource conf (Just consumeParametersSingle) myEventTypeName Nothing
      runConduit $ source .| headC
    liftIO $ isJust eventConsumed @=? True

  counter <- atomically $ readTVar deserializationFailureCounter
  liftIO $ 1 @=? counter

  where conf = conf'
               & setDeserializationFailureCallback deserializationFailureCb

        deserializationFailureCb _ _ =
          atomically $ modifyTVar deserializationFailureCounter (+ 1)

        deserializationFailureCounter = unsafePerformIO $ newTVarIO 0
