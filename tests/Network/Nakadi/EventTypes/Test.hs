{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.EventTypes.Test where

import           ClassyPrelude           hiding ( withAsync )

import           Control.Lens
import           Data.Function                  ( (&) )
import           Network.Nakadi
import           Network.Nakadi.EventTypes.CursorsLag.Test
import           Network.Nakadi.EventTypes.ShiftedCursors.Test
import           Network.Nakadi.EventTypes.BusinessEvents.Test
import qualified Network.Nakadi.Lenses         as L
import qualified Data.Vector                   as Vector
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.IO.Unsafe
import           UnliftIO.Async

testEventTypes :: Config App -> TestTree
testEventTypes conf = testGroup
  "EventTypes"
  [ testCase "EventTypesPrepare"            (testEventTypesPrepare conf)
  , testCase "EventTypesGet"                (testEventTypesGet conf)
  , testCase "EventTypesDeleteCreateAndGet" (testEventTypesDeleteCreateGet conf)
  , testCase "EventTypePartitionsGet"       (testEventTypePartitionsGet conf)
  , testCase "EventTypeCursorDistances0"    (testEventTypeCursorDistances0 conf)
  , testCase "EventTypeCursorDistances10" (testEventTypeCursorDistances10 conf)
  , testCase "EventTypePublishData"         (testEventTypePublishData conf)
  , testCase "EventTypeParseFlowId"         (testEventTypeParseFlowId conf)
  , testCase "EventTypeDeserializationFailureException"
             (testEventTypeDeserializationFailureException conf)
  , testCase "EventTypeDeserializationFailure"
             (testEventTypeDeserializationFailure conf)
  , testEventTypesShiftedCursors conf
  , testEventTypesCursorsLag conf
  , testBusinessEvents conf
  ]

testEventTypesPrepare :: Config App -> Assertion
testEventTypesPrepare conf = runApp . runNakadiT conf $ do
  subscriptions <- subscriptionsList Nothing Nothing
  let subscriptionIds = map (view L.id) $ subscriptions
  forM_ subscriptionIds subscriptionDelete

testEventTypesGet :: Config App -> Assertion
testEventTypesGet conf = runApp . runNakadiT conf $ void eventTypesList

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

  forM_ [1 .. 10] $ \_ -> do
    now <- liftIO getCurrentTime
    eid <- EventId <$> genRandomUUID
    eventsPublish myEventTypeName [myDataChangeEvent eid now]

  cursorPairs <- forM cursors $ \cursor@Cursor {..} -> do
    part <- eventTypePartition myEventTypeName _partition
    let cursor' = extractCursor part
    return (cursor, cursor')

  distances <- forM cursorPairs
    $ \(c, c') -> cursorDistance myEventTypeName c c'

  let totalDistances = sum distances
  liftIO $ totalDistances @=? 10

mySubscription :: SubscriptionRequest
mySubscription = SubscriptionRequest
  { _owningApplication    = "test-suite"
  , _eventTypes           = [EventTypeName "test.FOO"]
  , _consumerGroup        = Nothing
  , _subscriptionPosition = Nothing
  }

createMySubscription :: (MonadUnliftIO m, MonadNakadi b m) => m SubscriptionId
createMySubscription = do
  newSubscription <- subscriptionCreate mySubscription `catch` \case
    SubscriptionExistsAlready s -> pure s
    exn                         -> throwIO exn
  pure (newSubscription ^. L.id)

testEventTypePublishData :: Config App -> Assertion
testEventTypePublishData conf' = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  recreateEvent myEventType
  let event = DataChangeEvent
        { _payload  = Foo "Hello!"
        , _metadata = EventMetadata
          { _eid        = eid
          , _occurredAt = Timestamp now
          , _parentEids = Nothing
          , _partition  = Nothing
          }
        , _dataType = "test.FOO"
        , _dataOp   = DataOpUpdate
        }
  subscriptionId <- createMySubscription
  batchTv <- newTVarIO Nothing
  res <- try $ withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    subscriptionProcess subscriptionId (storeBatch batchTv)
  liftIO $ (Left TerminateConsumption) @=? res
  Just batch <- atomically $ readTVar batchTv
  let Just events = batch ^. L.events :: Maybe (Vector (DataChangeEvent Foo))
  liftIO $ True @=? (Vector.length events > 0)

  where conf = conf' & setBatchLimit 1 & setBatchFlushTimeout 1

storeBatch
  :: MonadIO m
  => TVar (Maybe (SubscriptionEventStreamBatch a))
  -> SubscriptionEventStreamBatch a
  -> m ()
storeBatch batchTv batch = do
  atomically $ writeTVar batchTv (Just batch)
  throwIO TerminateConsumption

testEventTypeParseFlowId :: Config App -> Assertion
testEventTypeParseFlowId conf = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  recreateEvent myEventType
  let event = DataChangeEvent
        { _payload  = Foo "Hello!"
        , _metadata = EventMetadata
          { _eid        = eid
          , _occurredAt = Timestamp now
          , _parentEids = Nothing
          , _partition  = Nothing
          }
        , _dataType = "test.FOO"
        , _dataOp   = DataOpUpdate
        }
      expectedFlowId = Just $ FlowId "12345"
  subscriptionId <- createMySubscription
  batchTv        <- newTVarIO Nothing
  res            <-
    try $ withAsync (delayedPublish expectedFlowId [event]) $ \asyncHandle -> do
      liftIO $ link asyncHandle
      subscriptionProcess subscriptionId (storeBatch batchTv)
  liftIO $ (Left TerminateConsumption) @=? res
  Just batch <- atomically $ readTVar batchTv
  let Just (e : _) =
        toList <$> (batch ^. L.events) :: Maybe [DataChangeEventEnriched Foo]
  liftIO $ expectedFlowId @=? e ^. L.metadata . L.flowId

testEventTypeDeserializationFailureException :: Config App -> Assertion
testEventTypeDeserializationFailureException conf' =
  runApp . runNakadiT conf $ do
    now <- liftIO getCurrentTime
    eid <- EventId <$> genRandomUUID
    recreateEvent myEventType
    let event = DataChangeEvent
          { _payload  = Foo "Hello!"
          , _metadata = EventMetadata
            { _eid        = eid
            , _occurredAt = Timestamp now
            , _parentEids = Nothing
            , _partition  = Nothing
            }
          , _dataType = "test.FOO"
          , _dataOp   = DataOpUpdate
          }
    subscriptionId                   <- createMySubscription
    res :: Either NakadiException () <-
      try $ withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
        liftIO $ link asyncHandle
        subscriptionProcess subscriptionId
          $ \(_batch :: SubscriptionEventStreamBatch ()) -> pure ()
    case res of
      Left (DeserializationFailure _ _) -> pure ()
      Left exn -> liftIO $ assertFailure $ "Unexpected exception: " <> show exn
      Right events ->
        liftIO $ assertFailure $ "Unexpected success: " <> show events
  where conf = conf' & setBatchLimit 1 & setBatchFlushTimeout 1

testEventTypeDeserializationFailure :: Config App -> Assertion
testEventTypeDeserializationFailure conf' = runApp . runNakadiT conf $ do
  now <- liftIO getCurrentTime
  eid <- EventId <$> genRandomUUID
  recreateEvent myEventType
  let event = DataChangeEvent
        { _payload  = Foo "Hello!"
        , _metadata = EventMetadata
          { _eid        = eid
          , _occurredAt = Timestamp now
          , _parentEids = Nothing
          , _partition  = Nothing
          }
        , _dataType = "test.FOO"
        , _dataOp   = DataOpUpdate
        }
  subscriptionId <- createMySubscription
  res <- try $ withAsync (delayedPublish Nothing [event]) $ \asyncHandle -> do
    liftIO $ link asyncHandle
    subscriptionProcess subscriptionId
      $ \(_batch :: SubscriptionEventStreamBatch WrongFoo) ->
          throwIO TerminateConsumption
  liftIO $ Left TerminateConsumption @=? res
  counter <- atomically $ readTVar deserializationFailureCounter
  liftIO $ 1 @=? counter
 where
  conf = conf' & setDeserializationFailureCallback deserializationFailureCb

  deserializationFailureCb _ _ =
    atomically $ modifyTVar deserializationFailureCounter (+ 1)

  deserializationFailureCounter = unsafePerformIO $ newTVarIO 0
