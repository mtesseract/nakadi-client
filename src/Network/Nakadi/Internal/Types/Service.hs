-- | Types modelling the Nakadi Service API.

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.Nakadi.Internal.Types.Service where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import           Data.String
import qualified Data.Text                          as Text
import           Data.Time
import           Data.Time.ISO8601
import           Data.UUID
import           Data.Vector                        (Vector)
import           GHC.Generics

import           Network.Nakadi.Internal.Json
import           Network.Nakadi.Internal.Types.Util

-- | CursorOffset

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

deriveJSON nakadiJsonOptions ''CursorToken

-- | Cursor

data Cursor = Cursor
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  } deriving (Eq, Ord, Hashable, Show, Generic)

deriveJSON nakadiJsonOptions ''Cursor

-- | ApplicationName

newtype ApplicationName = ApplicationName { unApplicationName :: Text }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString ApplicationName where
  fromString = ApplicationName . Text.pack

instance ToJSON ApplicationName where
  toJSON = String . unApplicationName

instance FromJSON ApplicationName where
  parseJSON (String name) = return $ ApplicationName name
  parseJSON invalid       = typeMismatch "ApplicationName" invalid

-- | SubscriptionCursor

data SubscriptionCursor = SubscriptionCursor
  { _partition   :: PartitionName
  , _offset      :: CursorOffset
  , _eventType   :: EventTypeName
  , _cursorToken :: Text
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionCursor

-- | SubscriptionCursorWithoutToken

data SubscriptionCursorWithoutToken = SubscriptionCursorWithoutToken
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  , _eventType :: EventTypeName
  } deriving (Show, Generic, Eq, Ord, Hashable)

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

deriveJSON nakadiJsonOptions ''Metadata

-- | Event

data Event a = Event
  { _payload  :: a -- Cannot be named '_data', as this this would
                   -- cause the lense 'data' to be created, which is a
                   -- reserved keyword.
  , _metadata :: Metadata
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''Event

-- | EventStreamBatch

data EventStreamBatch a = EventStreamBatch
  { _cursor :: Cursor
  , _events :: Maybe (Vector (Event a))
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''EventStreamBatch

-- | SubscriptionEventStreamBatch

data SubscriptionEventStreamBatch a = SubscriptionEventStreamBatch
  { _cursor :: SubscriptionCursor
  , _events :: Maybe (Vector (Event a))
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventStreamBatch

-- | EventId

newtype EventId = EventId { unEventId :: UUID }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EventId where
  toJSON = String . tshow . unEventId

instance FromJSON EventId where
  parseJSON = parseUUID "EventId" EventId

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

-- | CursorDistanceQuery

data CursorDistanceQuery = CursorDistanceQuery
  { _initialCursor :: Cursor
  , _finalCursor   :: Cursor
  } deriving (Show, Eq, Ord, Hashable, Generic)

deriveJSON nakadiJsonOptions ''CursorDistanceQuery

-- | CursorDistanceResult

newtype CursorDistanceResult = CursorDistanceResult
  { _distance :: Int64
  } deriving (Show, Eq, Ord, Hashable, Generic)

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

-- | PublishingStatus
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

-- | Step
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

-- | SchemaType

data SchemaType = SchemaTypeJson
  deriving (Eq, Show, Ord, Generic, Hashable)

instance ToJSON SchemaType where
  toJSON pos = case pos of
                 SchemaTypeJson   -> String "json_schema"

instance FromJSON SchemaType where
  parseJSON pos = case pos of
                    String "json_schema" -> return SchemaTypeJson
                    invalid              -> typeMismatch "SchemaType" invalid

-- | Type for the version of a schema.
newtype SchemaVersion = SchemaVersion { unSchemaVersion :: Text }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString SchemaVersion where
  fromString = SchemaVersion . Text.pack

instance ToJSON SchemaVersion where
  toJSON = String . unSchemaVersion

instance FromJSON SchemaVersion where
  parseJSON (String version) = return $ SchemaVersion version
  parseJSON invalid          = typeMismatch "SchemaVersion" invalid

-- | Type for the schema of an event type.
data EventTypeSchema = EventTypeSchema
  { _version    :: Maybe SchemaVersion
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

-- | PaginationLink
newtype PaginationLink = PaginationLink
  { _href :: Text
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''PaginationLink

-- | PaginationLinks

data PaginationLinks = PaginationLinks
  { _prev :: Maybe PaginationLink
  , _next :: Maybe PaginationLink
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''PaginationLinks

-- | EventTypeSchemasResponse

data EventTypeSchemasResponse = EventTypeSchemasResponse
  { __links :: PaginationLinks
  , _items  :: [EventTypeSchema]
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''EventTypeSchemasResponse

-- | Offset

newtype Offset = Offset { unOffset :: Int64 }
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | Limit

newtype Limit = Limit { unLimit :: Int64 }
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | PartitionState

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

-- | PartitionStat

data PartitionStat = PartitionStat
  { _partition        :: PartitionName
  , _state            :: PartitionState
  , _unconsumedEvents :: Int64
  , _streamId         :: StreamId
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''PartitionStat

-- | SubscriptionEventTypeStats

data SubscriptionEventTypeStats = SubscriptionEventTypeStats
  { _eventType  :: EventTypeName
  , _partitions :: [PartitionStat]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventTypeStats

-- | SubscriptionEventTypeStatsResult

newtype SubscriptionEventTypeStatsResult = SubscriptionEventTypeStatsResult
  { _items :: [SubscriptionEventTypeStats]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventTypeStatsResult

-- | Type for the category of an 'EventType'.

data EventTypeCategory = EventTypeCategoryUndefined
                       | EventTypeCategoryData
                       | EventTypeCategoryBusiness
                       deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EventTypeCategory where
  toJSON resultType = String $ case resultType of
    EventTypeCategoryUndefined -> "undefined"
    EventTypeCategoryData      -> "data"
    EventTypeCategoryBusiness  -> "business"

instance FromJSON EventTypeCategory where
  parseJSON category = case category of
    "undefined" -> return EventTypeCategoryUndefined
    "data"      -> return EventTypeCategoryData
    "business"  -> return EventTypeCategoryBusiness
    invalid     -> typeMismatch "EventTypeCategory" invalid

-- | Type for a partitioning strategy.

newtype PartitionStrategy = PartitionStrategy { unPartitionStrategy :: Text }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString PartitionStrategy where
  fromString = PartitionStrategy . Text.pack

instance ToJSON PartitionStrategy where
  toJSON = String . unPartitionStrategy

instance FromJSON PartitionStrategy where
  parseJSON (String strategy) = return $ PartitionStrategy strategy
  parseJSON invalid           = typeMismatch "PartitionStrategy" invalid

-- | Type for an enrichment stragey.

data EnrichmentStrategy = EnrichmentStrategyMetadata
                       deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EnrichmentStrategy where
  toJSON resultType = String $ case resultType of
    EnrichmentStrategyMetadata -> "metadata_enrichment"

instance FromJSON EnrichmentStrategy where
  parseJSON strategy = case strategy of
    "metadata_enrichment" -> return EnrichmentStrategyMetadata
    invalid               -> typeMismatch "EnrichmentStrategy" invalid

-- | Type for an event type compatibility mode.

data CompatibilityMode = CompatibilityModeCompatible
                       | CompatibilityModeForward
                       | CompatibilityModeNone
                       deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON CompatibilityMode where
  toJSON mode = String $ case mode of
    CompatibilityModeCompatible -> "compatible"
    CompatibilityModeForward    -> "forward"
    CompatibilityModeNone       -> "none"

instance FromJSON CompatibilityMode where
  parseJSON strategy = case strategy of
    "compatible" -> return CompatibilityModeCompatible
    "forward"    -> return CompatibilityModeForward
    "none"       -> return CompatibilityModeNone
    invalid      -> typeMismatch "CompatibilityMode" invalid

-- | Type for a partitioning key field.

newtype PartitionKeyField = PartitionKeyField { unPartitionKeyField :: Text }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString PartitionKeyField where
  fromString = PartitionKeyField . Text.pack

instance ToJSON PartitionKeyField where
  toJSON = String . unPartitionKeyField

instance FromJSON PartitionKeyField where
  parseJSON (String strategy) = return $ PartitionKeyField strategy
  parseJSON invalid           = typeMismatch "PartitionKeyField" invalid

-- | EventType

data EventType = EventType
  { _name                 :: EventTypeName
  , _owningApplication    :: Maybe ApplicationName
  , _category             :: Maybe EventTypeCategory
  , _enrichmentStrategies :: Maybe [EnrichmentStrategy]
  , _partitionStrategy    :: Maybe PartitionStrategy
  , _compatibilityMode    :: Maybe CompatibilityMode
  , _partitionKeyFields   :: Maybe [PartitionKeyField]
  , _schema               :: EventTypeSchema
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''EventType
