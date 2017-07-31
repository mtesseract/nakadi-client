-- | Types modelling the Nakadi Service API.

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Nakadi.Types.Service where

import           Nakadi.Internal.Prelude

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy    as ByteString.Lazy
import           Data.Hashable
import           Data.String
import qualified Data.Text               as Text
import           Data.Time
import           Data.Time.ISO8601
import           Data.UUID
import           Data.Vector             (Vector)
import           GHC.Generics
import           Nakadi.Types.Problem

import           Nakadi.Internal.Json
import           Nakadi.Types.Util

newtype CursorOffset = CursorOffset { unCursorOffset :: Text }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance IsString CursorOffset where
  fromString = CursorOffset . Text.pack

instance ToJSON CursorOffset where
  toJSON = String . unCursorOffset

instance FromJSON CursorOffset where
  parseJSON (String offset) = return $ CursorOffset offset
  parseJSON invalid         = typeMismatch "CursorOffset" invalid

-- | EventTypeName
newtype EventTypeName = EventTypeName
  { unEventTypeName :: Text
  } deriving (Eq, Show, Generic, Ord, Hashable)

instance IsString EventTypeName where
  fromString = EventTypeName . Text.pack

instance ToJSON EventTypeName where
  toJSON = String . unEventTypeName

instance FromJSON EventTypeName where
  parseJSON (String name) = return $ EventTypeName name
  parseJSON invalid       = typeMismatch "EventTypeName" invalid

-- | PartitionName
newtype PartitionName = PartitionName
  { unPartitionName :: Text
  } deriving (Eq, Show, Generic, Ord, Hashable)

instance IsString PartitionName where
  fromString = PartitionName . Text.pack

instance ToJSON PartitionName where
  toJSON = String . unPartitionName

instance FromJSON PartitionName where
  parseJSON (String name) = return $ PartitionName name
  parseJSON invalid       = typeMismatch "PartitionName" invalid

-- | CursorToken

newtype CursorToken = CursorToken Text deriving (Eq, Show, Ord)

instance IsString CursorToken where
  fromString = CursorToken . Text.pack

-- | Cursor

data Cursor = Cursor
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  } deriving (Eq, Ord, Hashable, Show, Generic)



-- | OwningApplication

newtype ApplicationName = ApplicationName { unApplicationName :: Text }
  deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''ApplicationName

-- | SubscriptionCursor

data SubscriptionCursor = SubscriptionCursor
  { _partition   :: PartitionName
  , _offset      :: CursorOffset
  , _eventType   :: EventTypeName
  , _cursorToken :: Text
  } deriving (Show, Eq, Ord, Generic)

-- | SubscriptionCursorWithoutToken

data SubscriptionCursorWithoutToken = SubscriptionCursorWithoutToken
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  , _eventType :: EventTypeName
  } deriving (Show, Generic, Eq, Ord, Hashable)

-- -- | Partition

-- newtype Partition = Partition
--   { unPartition :: Text
--   } deriving (Show, Eq, Ord, Hashable, Generic)

-- instance ToJSON Partition where
--   toJSON = String . unPartition

-- instance FromJSON Partition where
--   parseJSON (String partition) = return $ Partition partition
--   parseJSON invalid            = typeMismatch "Partition" invalid

-- | Offset

deriveJSON nakadiJsonOptions ''CursorToken

deriveJSON nakadiJsonOptions ''Cursor

deriveJSON nakadiJsonOptions ''SubscriptionCursor

deriveJSON nakadiJsonOptions ''SubscriptionCursorWithoutToken

-- | SubscriptionCursorCommit

newtype SubscriptionCursorCommit = SubscriptionCursorCommit
  { _items :: [SubscriptionCursor]
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionCursorCommit

-- | CursorCommit

newtype CursorCommit = CursorCommit
  { _items :: [Cursor]
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''CursorCommit

-- | SubscriptionId

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: UUID }
  deriving (Eq, Show, Ord)

instance ToJSON SubscriptionId where
  toJSON = String . tshow . unSubscriptionId

instance FromJSON SubscriptionId where
  parseJSON = parseUUID "SubscriptionId" SubscriptionId

-- | StreamId

newtype StreamId = StreamId { unStreamId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON StreamId where
  toJSON = String . unStreamId

instance FromJSON StreamId where
  parseJSON (String s) = return $ StreamId s
  parseJSON invalid    = typeMismatch "StreamId" invalid

-- | SubscriptionEventStream

data SubscriptionEventStream = SubscriptionEventStream
  { _streamId       :: StreamId
  , _subscriptionId :: SubscriptionId
  } deriving (Show)

-- | EventStreamBatch

data EventStreamBatch a = EventStreamBatch
  { _cursor :: Cursor
  , _events :: Maybe (Vector (Event a))
  } deriving (Show, Generic)

-- | SubscriptionEventStreamBatch

data SubscriptionEventStreamBatch a = SubscriptionEventStreamBatch
  { _cursor :: SubscriptionCursor
  , _events :: Maybe (Vector (Event a))
  } deriving (Show, Generic)

-- | EventId

newtype EventId = EventId { unEventId :: UUID }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EventId where
  toJSON = String . tshow . unEventId

instance FromJSON EventId where
  parseJSON = parseUUID "EventId" EventId

-- | Timestamp

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Timestamp where
  hashWithSalt salt = hashWithSalt salt . tshow . unTimestamp

instance ToJSON Timestamp where
  toJSON = String . tshow . unTimestamp

instance FromJSON Timestamp where
  parseJSON s@(String timestamp) = case parseISO8601 (Text.unpack timestamp) of
                           Just t  -> return $ Timestamp t
                           Nothing -> typeMismatch "Timestamp" s
  parseJSON invalid     = typeMismatch "TimestampId" invalid

-- | Metadata

data Metadata = Metadata
  { _occurredAt :: Timestamp
  , _eid        :: Text
  , _eventType  :: Text
  } deriving (Eq, Show, Generic)

-- | Event

data Event a = Event
  { _payload  :: a -- Cannot be named '_data', as this this would
                   -- cause the lense 'data' to be created, which is a
                   -- reserved keyword.
  , _metadata :: Metadata
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''Metadata

deriveJSON nakadiJsonOptions ''Event

-- | Partition

data Partition = Partition
  { _oldestAvailableOffset :: CursorOffset
  , _newestAvailableOffset :: CursorOffset
  , _partition             :: PartitionName
  , _unconsumedEvents      :: Maybe Int64
  } deriving (Show)

deriveJSON nakadiJsonOptions ''Partition

-- | ShiftedCursor

data ShiftedCursor = ShiftedCursor
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  , _shift     :: Int64
  } deriving (Eq, Ord, Show)

deriveJSON nakadiJsonOptions ''ShiftedCursor

deriveJSON nakadiJsonOptions ''EventStreamBatch

deriveJSON nakadiJsonOptions ''SubscriptionEventStreamBatch

-- | CursorDistanceQuery

data CursorDistanceQuery = CursorDistanceQuery
  { _initialCursor :: Cursor
  , _finalCursor   :: Cursor
  } deriving (Show, Eq, Ord, Hashable, Generic)

-- | CursorDistanceResult

newtype CursorDistanceResult = CursorDistanceResult
  { _distance :: Int64
  } deriving (Show, Eq, Ord, Hashable, Generic)

deriveJSON nakadiJsonOptions ''CursorDistanceQuery

deriveJSON nakadiJsonOptions ''CursorDistanceResult

-- | SubscriptionPosition

data SubscriptionPosition = SubscriptionPositionBegin
                          | SubscriptionPositionEnd
                          | SubscriptionPositionCursors
                          deriving (Show, Eq, Ord)

instance ToJSON SubscriptionPosition where
  toJSON pos = case pos of
                 SubscriptionPositionBegin   -> "begin"
                 SubscriptionPositionEnd     -> "end"
                 SubscriptionPositionCursors -> "cursors"

instance FromJSON SubscriptionPosition where
  parseJSON pos = case pos of
                    "begin"   -> return SubscriptionPositionBegin
                    "end"     -> return SubscriptionPositionEnd
                    "cursors" -> return SubscriptionPositionCursors
                    invalid   -> typeMismatch "SubscriptionPosition" invalid



-- | Subscription

data Subscription = Subscription
  { _id                :: Maybe Text
  , _owningApplication :: Text
  , _eventTypes        :: [EventTypeName]
  , _consumerGroup     :: Maybe Text
  , _createdAt         :: Maybe Timestamp
  , _readFrom          :: Maybe SubscriptionPosition
  , _initialCursors    :: Maybe [SubscriptionCursorWithoutToken]
  } deriving (Show, Eq, Ord)

deriveJSON nakadiJsonOptions ''Subscription

data PublishingStatus = PublishingStatusSubmitted
                      | PublishingStatusFailed
                      | PublishingStatusAborted
                      deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON PublishingStatus where
  toJSON status = case status of
                    PublishingStatusSubmitted -> "submitted"
                    PublishingStatusFailed    -> "failed"
                    PublishingStatusAborted   -> "aborted"

instance FromJSON PublishingStatus where
  parseJSON status = case status of
                       "submitted" -> return PublishingStatusSubmitted
                       "failed"    -> return PublishingStatusFailed
                       "aborted"   -> return PublishingStatusAborted
                       invalid     -> typeMismatch "PublishingStatus" invalid

data Step = StepNone
          | StepValidating
          | StepPartitioning
          | StepEnriching
          | StepPublishing
          deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON Step where
  toJSON step = case step of
                  StepNone         -> "none"
                  StepValidating   -> "validating"
                  StepPartitioning -> "partitioning"
                  StepEnriching    -> "enriching"
                  StepPublishing   -> "publishing"

instance FromJSON Step where
  parseJSON step = case step of
                     "none"       -> return StepNone
                     "validating" -> return StepValidating
                     "enriching"  -> return StepEnriching
                     "publishing" -> return StepPublishing
                     invalid      -> typeMismatch "Step" invalid

-- | BatchItemResponse

data BatchItemResponse = BatchItemResponse
  { _eid              :: Maybe EventId
  , _publishingStatus :: PublishingStatus
  , _step             :: Maybe Step
  } deriving (Show, Eq, Ord, Generic, Hashable)

-- | StreamKeepAliveLimit

newtype StreamKeepAliveLimit = StreamKeepAliveLimit { unStreamKeepAliveLimit :: Int32 }
  deriving (Show, Eq, Ord)

instance ToJSON StreamKeepAliveLimit where
  toJSON = Number . fromIntegral . unStreamKeepAliveLimit

instance FromJSON StreamKeepAliveLimit where
  parseJSON = parseInteger "StreamKeepAliveLimit" StreamKeepAliveLimit

-- | BatchFlushTimeout

newtype BatchFlushTimeout = BatchFlushTimeout { unBatchFlushTimeout :: Int32 }
  deriving (Show, Eq, Ord)

instance ToJSON BatchFlushTimeout where
  toJSON = Number . fromIntegral . unBatchFlushTimeout

instance FromJSON BatchFlushTimeout where
  parseJSON = parseInteger "BatchFlushTimeout" BatchFlushTimeout

-- | CursorCommitResultType

data CursorCommitResultType = CursorCommitResultCommitted
                            | CursorCommitResultOutdated
                            deriving (Show, Eq, Ord)

instance ToJSON CursorCommitResultType where
  toJSON resultType = String $ case resultType of
    CursorCommitResultCommitted -> "committed"
    CursorCommitResultOutdated  -> "outdated"

instance FromJSON CursorCommitResultType where
  parseJSON resultType = case resultType of
    "committed" -> return CursorCommitResultCommitted
    "outdated"  -> return CursorCommitResultOutdated
    invalid     -> typeMismatch "CursorCommitResultType" invalid

-- | CursorCommitResult

data CursorCommitResult = CursorCommitResult
  { _cursor :: SubscriptionCursor
  , _result :: CursorCommitResultType
  } deriving (Show, Eq, Ord)

deriveJSON nakadiJsonOptions ''CursorCommitResult

data LibException = BatchPartiallySubmitted [BatchItemResponse]
                  | BatchValidationFailure [BatchItemResponse]
                  | ClientNotAuthenticated Problem
                  | AccessForbidden Problem
                  | UnprocessableEntity Problem
                  | Conflict Problem
                  | DeserializationFailure ByteString.Lazy.ByteString
                  | UnexpectedResponse (Response ())
                  | NotFound Problem
                  | TooManyRequests Problem
                  | BadRequest Problem
                  | SubscriptionNotFound Problem
                  | CursorAlreadyCommitted [CursorCommitResult]
                  | CursorResetInProgress Problem
                  | EventTypeNotFound Problem
                  | SubscriptionExistsAlready Subscription
                  | RequestModificationException SomeException
                  | CursorDistanceNoResult
                  deriving (Show, Typeable)

instance Exception LibException

-- | Schemas

data SchemaType = SchemaTypeJson
  deriving (Eq, Show, Ord, Generic, Hashable)

instance ToJSON SchemaType where
  toJSON pos = case pos of
                 SchemaTypeJson   -> String "json_schema"

instance FromJSON SchemaType where
  parseJSON pos = case pos of
                    String "json_schema" -> return SchemaTypeJson
                    invalid              -> typeMismatch "SchemaType" invalid

data EventTypeSchema = EventTypeSchema
  { _version    :: Maybe Text
  , _createdAt  :: Maybe Timestamp
  , _schemaType :: SchemaType
  , _schema     :: Text
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions {
  fieldLabelModifier = makeFieldRenamer [ ("_version",    "version")
                                        , ("_createdAt",  "created_at")
                                        , ("_schemaType", "type")
                                        , ("_schema",     "schema") ]
  }  ''EventTypeSchema

newtype PaginationLink = PaginationLink
  { _href :: Text
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''PaginationLink

data PaginationLinks = PaginationLinks
  { _prev :: Maybe PaginationLink
  , _next :: Maybe PaginationLink
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''PaginationLinks

data EventTypeSchemasResponse = EventTypeSchemasResponse
  { __links :: PaginationLinks
  , _items  :: [EventTypeSchema]
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''EventTypeSchemasResponse

newtype SchemaVersion = SchemaVersion { unSchemaVersion :: Text }
  deriving (Show, Eq, Ord, Generic)

instance IsString SchemaVersion where
  fromString = SchemaVersion . Text.pack

deriveJSON nakadiJsonOptions ''SchemaVersion

newtype Offset = Offset { unOffset :: Int64 }
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype Limit = Limit { unLimit :: Int64 }
  deriving (Show, Eq, Ord, Generic, Hashable)

data PartitionState = PartitionStateUnassigned
                    | PartitionStateReassigning
                    | PartitionStateAssigned
                    deriving (Show, Eq, Ord)

instance ToJSON PartitionState where
  toJSON = \case
    PartitionStateUnassigned  -> String "unassigned"
    PartitionStateAssigned    -> String "assigned"
    PartitionStateReassigning -> String "reassigning"

instance FromJSON PartitionState where
  parseJSON = \case
    String "unassigned"  -> return PartitionStateUnassigned
    String "assigned"    -> return PartitionStateAssigned
    String "reassigning" -> return PartitionStateReassigning
    invalid              -> typeMismatch "PartitionState" invalid

data PartitionStat = PartitionStat
  { _partition        :: PartitionName
  , _state            :: PartitionState
  , _unconsumedEvents :: Int64
  , _streamId         :: StreamId
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''PartitionStat

data SubscriptionEventTypeStats = SubscriptionEventTypeStats
  { _eventType  :: EventTypeName
  , _partitions :: [PartitionStat]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventTypeStats

newtype SubscriptionEventTypeStatsResult = SubscriptionEventTypeStatsResult
  { _items :: [SubscriptionEventTypeStats]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventTypeStatsResult

-- | EventType

data EventType = EventType
  { _name                 :: Text
  , _owningApplication    :: Maybe Text
  , _category             :: Maybe Text
  , _enrichmentStrategies :: Maybe [Text]
  , _partitionStrategy    :: Maybe Text
  , _compatibilityMode    :: Maybe Text
  , _partitionKeyFields   :: Maybe [Text]
  , _schema               :: EventTypeSchema
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''EventType
