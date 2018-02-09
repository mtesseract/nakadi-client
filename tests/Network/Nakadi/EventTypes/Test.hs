{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.EventTypes.Test where

import           ClassyPrelude                                 hiding
                                                                (withAsync)

import           Conduit                                       hiding
                                                                (runResourceT)
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
import           UnliftIO.Async

testEventTypes :: Config App -> TestTree
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

testEventTypesPrepare :: Config App -> Assertion
testEventTypesPrepare conf = runApp . runNakadiT conf $ do
  subscriptions <- subscriptionsList Nothing Nothing
  let subscriptionIds = catMaybes . map (view L.id) $ subscriptions
  forM_ subscriptionIds subscriptionDelete

testEventTypesGet :: Config App -> Assertion
testEventTypesGet conf = runApp . runNakadiT conf $
  void eventTypesList

testEventTypesDeleteCreateGet :: Config App -> Assertion
testEventTypesDeleteCreateGet conf = runApp . runNakadiT conf $ do
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  myEventTypes <- filterMyEvent <$> eventTypesList
  liftIO $ length myEventTypes @=? 1
  eventTypeDelete myEventTypeName
  myEventTypes' <- filterMyEvent <$> eventTypesList
  liftIO $ length myEventTypes' @=? 0

  where filterMyEvent = filter ((myEventTypeName ==) . (view L.name))

testEventTypePartitionsGet :: Config App -> Assertion
testEventTypePartitionsGet conf = runApp . runNakadiT conf $ do
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  void $ eventTypePartitions myEventTypeName

testEventTypeCursorDistances0 :: Config App -> Assertion
testEventTypeCursorDistances0 conf = runApp . runNakadiT conf $ do
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  partitions <- eventTypePartitions myEventTypeName
  let cursors = map extractCursor partitions
  forM_ cursors $ \cursor -> do
    distance <- cursorDistance myEventTypeName cursor cursor
    liftIO $ distance @=? 0

testEventTypeCursorDistances10 :: Config App -> Assertion
testEventTypeCursorDistances10 conf = runApp . runNakadiT conf $ do
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  partitions <- eventTypePartitions myEventTypeName
  let cursors = map extractCursor partitions

  forM_ [1..10] $ \_ -> do
    now <- liftIO getCurrentTime
    eid <- EventId <$> genRandomUUID
    eventsPublish myEventTypeName Nothing [myDataChangeEvent eid now]

  cursorPairs <- forM cursors $ \cursor@Cursor { .. } -> do
    part <- eventTypePartition myEventTypeName _partition
    let cursor' = extractCursor part
    return (cursor, cursor')

  distances <- forM cursorPairs $ \(c, c') -> do
    cursorDistance myEventTypeName c c'

  let totalDistances = sum distances
  liftIO $ totalDistances @=? 10

consumeParametersSingle :: ConsumeParameters
consumeParametersSingle = defaultConsumeParameters
                          & setBatchLimit 1
                          & setBatchFlushTimeout 1

testEventTypePublishData :: Config App -> Assertion
testEventTypePublishData conf = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = EventMetadata { _eid = eid
                                                          , _occurredAt = Timestamp now
                                                          , _parentEids = Nothing
                                                          , _partition  = Nothing
                                                          }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
  withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch Foo) <-
      eventsProcessConduit (Just consumeParametersSingle) myEventTypeName Nothing headC
    liftIO $ isJust eventConsumed @=? True

testEventTypeParseFlowId :: Config App -> Assertion
testEventTypeParseFlowId conf = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = EventMetadata { _eid = eid
                                                          , _occurredAt = Timestamp now
                                                          , _parentEids = Nothing
                                                          , _partition  = Nothing
                                                          }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
      expectedFlowId = Just $ FlowId "12345"
  withAsync (delayedPublish expectedFlowId [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch (DataChangeEventEnriched Foo)) <-
      eventsProcessConduit (Just consumeParametersSingle) myEventTypeName Nothing headC
    liftIO $ isJust eventConsumed @=? True
    let events = eventConsumed >>= (\batch -> batch^.L.events)
    liftIO $ isJust events @=? True

    liftIO $ case events of
      Nothing -> assertFailure "Received no events"
      Just v -> case toList v of
        [DataChangeEventEnriched _ x _ _] ->
          x^.L.flowId @=? expectedFlowId
        _ -> assertFailure "Received not a singleton event list"

testEventTypeDeserializationFailure :: Config App -> Assertion
testEventTypeDeserializationFailure conf' = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate myEventType
  let event = DataChangeEvent { _payload = Foo "Hello!"
                              , _metadata = EventMetadata { _eid = eid
                                                          , _occurredAt = Timestamp now
                                                          , _parentEids = Nothing
                                                          , _partition  = Nothing
                                                          }
                              , _dataType = "test.FOO"
                              , _dataOp = DataOpUpdate
                              }
  withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    eventConsumed :: Maybe (EventStreamBatch WrongFoo) <-
      eventsProcessConduit (Just consumeParametersSingle) myEventTypeName Nothing headC
    liftIO $ isJust eventConsumed @=? True

  counter <- atomically $ readTVar deserializationFailureCounter
  liftIO $ 1 @=? counter

  where conf = conf'
               & setDeserializationFailureCallback deserializationFailureCb

        deserializationFailureCb _ _ =
          atomically $ modifyTVar deserializationFailureCounter (+ 1)

        deserializationFailureCounter = unsafePerformIO $ newTVarIO 0
