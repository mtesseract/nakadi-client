{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Nakadi.Tests.Common where

import           ClassyPrelude

import           Data.Aeson
import           Data.UUID      (UUID)
import           Network.Nakadi
import           System.Random

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
  { _version = Just "0.1"
  , _createdAt = Nothing
  , _schemaType = SchemaTypeJson
  , _schema = "{ \"properties\": {\"fortune\": {\"type\": \"string\"} }, \"required\": [\"fortune\"] }"
  }

myEventType :: EventType
myEventType = EventType
  { _name = myEventTypeName
  , _owningApplication = Just "test-suite"
  , _category = Just EventTypeCategoryData
  , _enrichmentStrategies = Just [EnrichmentStrategyMetadata]
  , _partitionStrategy = Just "hash"
  , _compatibilityMode = Just CompatibilityModeForward
  , _partitionKeyFields = Just ["fortune"]
  , _schema = myEventTypeSchema
  , _defaultStatistic = Nothing
  , _options = Nothing
  }

ignoreExnNotFound :: MonadThrow m => a -> NakadiException -> m a
ignoreExnNotFound a (EventTypeNotFound _) = return a
ignoreExnNotFound _ exn                   = throw exn

extractCursor :: Partition -> Cursor
extractCursor Partition { ..} =
  Cursor { _partition = _partition
         , _offset    = _newestAvailableOffset }

myDataChangeEvent :: Text -> UTCTime -> DataChangeEvent Foo
myDataChangeEvent eid now =  DataChangeEvent
  { _payload = Foo "Hello!"
  , _metadata = Metadata { _eid        = eid
                         , _occurredAt = Timestamp now
                         , _parentEids = Nothing
                         , _partition  = Nothing
                         }
  , _dataType = "test.FOO"
  , _dataOp = DataOpUpdate
  }

genRandomUUID :: IO UUID
genRandomUUID = randomIO

recreateEvent :: Config -> EventTypeName -> EventType -> IO ()
recreateEvent conf eventTypeName eventType = do
  eventTypeDelete conf eventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate conf eventType

delayedPublish :: (ToJSON a) => Config -> Maybe FlowId -> [a] -> IO ()
delayedPublish conf flowId events  = do
  threadDelay (10^6)
  eventPublish conf myEventTypeName flowId events