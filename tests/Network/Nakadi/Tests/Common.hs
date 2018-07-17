{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Network.Nakadi.Tests.Common where

import           ClassyPrelude

import           Control.Exception.Safe         ( MonadThrow
                                                , throwM
                                                )
import           Control.Lens
import           Control.Monad.Logger
import           Data.Aeson
import           Data.List.Split                ( chunksOf )
import qualified Data.Text                     as Text
import           Data.UUID                      ( UUID )
import           Network.Nakadi
import qualified Network.Nakadi.Lenses         as L
import           System.Random
import           UnliftIO.Concurrent

data TerminateConsumption = TerminateConsumption deriving (Eq, Show, Typeable)

instance Exception TerminateConsumption

type App = LoggingT (ReaderT () IO)

runApp :: App a -> IO a
runApp = flip runReaderT () . runStdoutLoggingT

data Foo = Foo { fortune :: Text } deriving (Show, Eq, Generic)

deriving instance FromJSON Foo
deriving instance ToJSON Foo

data WrongFoo = WrongFoo { fortune :: Int } deriving (Show, Eq, Generic)

deriving instance FromJSON WrongFoo
deriving instance ToJSON WrongFoo

myEventTypeName :: EventTypeName
myEventTypeName = "test.FOO"

myEventTypeSchema :: EventTypeSchema
myEventTypeSchema = EventTypeSchema
  { _version    = Just "0.1"
  , _createdAt  = Nothing
  , _schemaType = SchemaTypeJson
  , _schema = "{ \"properties\": {\"fortune\": {\"type\": \"string\"} }, \"required\": [\"fortune\"] }"
  }

myEventType :: EventType
myEventType = EventType
  { _name                 = myEventTypeName
  , _owningApplication    = Just "test-suite"
  , _category             = Just EventTypeCategoryData
  , _enrichmentStrategies = Just [EnrichmentStrategyMetadata]
  , _partitionStrategy    = Just "hash"
  , _compatibilityMode    = Just CompatibilityModeForward
  , _partitionKeyFields   = Just ["fortune"]
  , _schema               = myEventTypeSchema
  , _defaultStatistic     = Nothing
  , _options              = Nothing
  }

ignoreExnNotFound :: MonadThrow m => a -> NakadiException -> m a
ignoreExnNotFound a (EventTypeNotFound _) = return a
ignoreExnNotFound _ exn                   = throwM exn

extractCursor :: Partition -> Cursor
extractCursor Partition {..} = Cursor {_partition = _partition, _offset = _newestAvailableOffset}

myDataChangeEvent :: EventId -> UTCTime -> DataChangeEvent Foo
myDataChangeEvent eid now = DataChangeEvent
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


genMyDataChangeEvent :: MonadIO m => m (DataChangeEvent Foo)
genMyDataChangeEvent = do
  eid <- genRandomUUID
  now <- liftIO getCurrentTime
  pure DataChangeEvent
    { _payload  = Foo "Hello!"
    , _metadata = EventMetadata
      { _eid        = EventId eid
      , _occurredAt = Timestamp now
      , _parentEids = Nothing
      , _partition  = Nothing
      }
    , _dataType = "test.FOO"
    , _dataOp   = DataOpUpdate
    }

genMyDataChangeEventIdx :: MonadIO m => Int -> m (DataChangeEvent Foo)
genMyDataChangeEventIdx idx = do
  eid <- genRandomUUID
  now <- liftIO getCurrentTime
  pure DataChangeEvent
    { _payload  = Foo ("Hello " ++ Text.pack (show idx))
    , _metadata = EventMetadata
      { _eid        = EventId eid
      , _occurredAt = Timestamp now
      , _parentEids = Nothing
      , _partition  = Nothing
      }
    , _dataType = "test.FOO"
    , _dataOp   = DataOpUpdate
    }

genRandomUUID :: MonadIO m => m UUID
genRandomUUID = liftIO randomIO

recreateEvent :: (MonadUnliftIO m, MonadNakadi b m) => EventType -> m ()
recreateEvent eventType = do
  let eventTypeName = eventType ^. L.name
  subscriptionIds <- subscriptionsList Nothing (Just [eventTypeName]) <&> map (view L.id)
  mapM_ subscriptionDelete subscriptionIds
  eventTypeDelete eventTypeName `catch` ignoreExnNotFound ()
  eventTypeCreate eventType

delayedPublish :: (MonadNakadi b m, MonadIO m, ToJSON a) => Maybe FlowId -> [a] -> m ()
delayedPublish maybeFlowId events = do
  liftIO $ threadDelay (10 ^ 6)
  let flowId = fromMaybe (FlowId "shalom") maybeFlowId
  config <- nakadiAsk <&> setFlowId flowId
  -- Publish events in batches.
  runNakadiT config $ forM_ (chunksOf 100 events) (eventsPublish myEventTypeName)
