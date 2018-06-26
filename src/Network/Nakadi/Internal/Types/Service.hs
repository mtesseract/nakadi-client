{-|
Module      : Network.Nakadi.Internal.Types.Service
Description : Nakadi Client Service Types (Internal)
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Types modelling the Nakadi Service API.
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.Nakadi.Internal.Types.Service where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.HashMap.Strict           as HashMap
import           Data.String
import qualified Data.Text                     as Text
import           Data.Time
import           Data.Time.ISO8601
import           Data.UUID
import           Data.Vector                    ( Vector )
import           GHC.Generics
import qualified Data.HashMap.Strict           as HashMap

import           Network.Nakadi.Internal.Json
import           Network.Nakadi.Internal.Types.Util

-- | Type for cursor offsets.
newtype CursorOffset = CursorOffset
  { unCursorOffset :: Text -- ^ Opaque Cursor Offset, do not parse
                           -- this
  }
  deriving (Show, Eq, Ord, Hashable, Generic)

instance IsString CursorOffset where
  fromString = CursorOffset . Text.pack

instance ToJSON CursorOffset where
  toJSON = String . unCursorOffset

instance FromJSON CursorOffset where
  parseJSON (String offset) = return $ CursorOffset offset
  parseJSON invalid         = typeMismatch "CursorOffset" invalid

-- | Type for event type names.
newtype EventTypeName = EventTypeName
  { unEventTypeName :: Text -- ^ Wrapped Event Type Name
  } deriving (Eq, Show, Generic, Ord, Hashable)

instance IsString EventTypeName where
  fromString = EventTypeName . Text.pack

instance ToJSON EventTypeName where
  toJSON = String . unEventTypeName

instance FromJSON EventTypeName where
  parseJSON (String name) = return $ EventTypeName name
  parseJSON invalid       = typeMismatch "EventTypeName" invalid

-- | Type for partition names.
newtype PartitionName = PartitionName
  { unPartitionName :: Text -- ^ Wrapped Partition Name
  } deriving (Eq, Show, Generic, Ord, Hashable)

instance IsString PartitionName where
  fromString = PartitionName . Text.pack

instance ToJSON PartitionName where
  toJSON = String . unPartitionName

instance FromJSON PartitionName where
  parseJSON (String name) = return $ PartitionName name
  parseJSON invalid       = typeMismatch "PartitionName" invalid

-- | Type for cursor tokens.

newtype CursorToken = CursorToken Text deriving (Eq, Show, Ord)

instance IsString CursorToken where
  fromString = CursorToken . Text.pack

deriveJSON nakadiJsonOptions ''CursorToken

-- | Type for cursors.

data Cursor = Cursor
  { _partition :: PartitionName
  , _offset    :: CursorOffset
  } deriving (Eq, Ord, Hashable, Show, Generic)

deriveJSON nakadiJsonOptions ''Cursor

-- | Type for  application names.

newtype ApplicationName = ApplicationName { unApplicationName :: Text }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance IsString ApplicationName where
  fromString = ApplicationName . Text.pack

instance ToJSON ApplicationName where
  toJSON = String . unApplicationName

instance FromJSON ApplicationName where
  parseJSON (String name) = return $ ApplicationName name
  parseJSON invalid       = typeMismatch "ApplicationName" invalid

-- | Type fo rsubscription cursors.

data SubscriptionCursor = SubscriptionCursor
  { _partition   :: PartitionName -- ^ Partition Name of this cursor
  , _offset      :: CursorOffset -- ^ Offset of this cursor
  , _eventType   :: EventTypeName -- ^ Event Type Name of this cursor
  , _cursorToken :: Text -- ^ Token of this cursor
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionCursor

-- | Type for subscription cursors without token.

data SubscriptionCursorWithoutToken = SubscriptionCursorWithoutToken
  { _partition :: PartitionName -- ^ Partition Name of this cursor
  , _offset    :: CursorOffset -- ^ Offset of this cursor
  , _eventType :: EventTypeName -- ^ Event Type Name of this cursor
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''SubscriptionCursorWithoutToken

-- | Type for commit object for subscription cursor committing.

newtype SubscriptionCursorCommit = SubscriptionCursorCommit
  { _items :: [SubscriptionCursor] -- ^ List of cursors to commit
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionCursorCommit

-- | Type for commit objects for cursor committing.

newtype CursorCommit = CursorCommit
  { _items :: [Cursor] -- ^ List of cursors to commit
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''CursorCommit

-- | Type for subscription IDs.

newtype SubscriptionId = SubscriptionId
  { unSubscriptionId :: UUID -- ^ Wrapped UUID
  }
  deriving (Eq, Show, Ord, Generic, Hashable)

instance ToJSON SubscriptionId where
  toJSON = String . tshow . unSubscriptionId

instance FromJSON SubscriptionId where
  parseJSON = parseUUID "SubscriptionId" SubscriptionId

-- | Type for stream IDs.

newtype StreamId = StreamId
  { unStreamId :: Text -- ^ Wrapped Stream ID
  }
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

-- | Type for timestamps.

newtype Timestamp = Timestamp
  { unTimestamp :: UTCTime -- ^ Wrapped UTC Timestamp
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Timestamp where
  hashWithSalt salt = hashWithSalt salt . tshow . unTimestamp

instance ToJSON Timestamp where
  toJSON = String . Text.pack . formatISO8601Millis . unTimestamp

instance FromJSON Timestamp where
  parseJSON s@(String timestamp) = case parseISO8601 (Text.unpack timestamp) of
                           Just t  -> return $ Timestamp t
                           Nothing -> typeMismatch "Timestamp" s
  parseJSON invalid     = typeMismatch "TimestampId" invalid

-- | A Flow ID.

newtype FlowId = FlowId
  { unFlowId :: Text -- ^ Wrapped Flow ID
  }
  deriving (Show, Eq, Ord, Generic)

instance IsString FlowId where
  fromString = FlowId . Text.pack

instance ToJSON FlowId where
  toJSON = String . unFlowId

instance FromJSON FlowId where
  parseJSON (String s) = return $ FlowId s
  parseJSON invalid    = typeMismatch "FlowId" invalid

-- | ID of an Event

newtype EventId = EventId
  { unEventId :: UUID -- ^ Wrapped UUID
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EventId where
  toJSON = String . tshow . unEventId

instance FromJSON EventId where
  parseJSON = parseUUID "EventId" EventId

-- | Partition Data

data Partition = Partition
  { _oldestAvailableOffset :: CursorOffset -- ^ Oldest available
                                           -- cursor offset for this
                                           -- partition
  , _newestAvailableOffset :: CursorOffset -- ^ Newest available
                                           -- cursor offset for this
                                           -- partition
  , _partition             :: PartitionName -- ^ Name of the partition
  , _unconsumedEvents      :: Maybe Int64 -- ^ Number of unconsumed
                                          -- Events
  } deriving (Show)

deriveJSON nakadiJsonOptions ''Partition

-- | Type for shift-cursor queries.

data ShiftedCursor = ShiftedCursor
  { _partition :: PartitionName -- ^ Partition of the cursor to shift
  , _offset    :: CursorOffset -- ^ Offset of the cursor to shift
  , _shift     :: Int64 -- ^ Shift by this number.
  } deriving (Eq, Ord, Show)

deriveJSON nakadiJsonOptions ''ShiftedCursor

-- | Type for cursor-distance queries. Represents the request to
-- compute the distance between initial cursor and final cursor.

data CursorDistanceQuery = CursorDistanceQuery
  { _initialCursor :: Cursor -- ^ Initial Cursor
  , _finalCursor   :: Cursor -- ^ Final cursor.
  } deriving (Show, Eq, Ord, Hashable, Generic)

deriveJSON nakadiJsonOptions ''CursorDistanceQuery

-- | Type for results of cursor-distance-queries.

newtype CursorDistanceResult = CursorDistanceResult
  { _distance :: Int64
  } deriving (Show, Eq, Ord, Hashable, Generic)

deriveJSON nakadiJsonOptions ''CursorDistanceResult

-- | This type models the "read_from" field contained in subscription objects.

data SubscriptionReadFrom = SubscriptionReadFromBegin
                          | SubscriptionReadFromEnd
                          | SubscriptionReadFromCursors
                          deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON SubscriptionReadFrom where
  toJSON = \case
    SubscriptionReadFromBegin   -> "begin"
    SubscriptionReadFromEnd     -> "end"
    SubscriptionReadFromCursors -> "cursors"

instance FromJSON SubscriptionReadFrom where
  parseJSON = \case
    "begin"   -> return SubscriptionReadFromBegin
    "end"     -> return SubscriptionReadFromEnd
    "cursors" -> return SubscriptionReadFromCursors
    invalid   -> typeMismatch "SubscriptionReadFrom" invalid

-- | Type modelling a subscription position.

data SubscriptionPosition = SubscriptionPositionBegin
                          | SubscriptionPositionEnd
                          | SubscriptionPositionCursors [SubscriptionCursorWithoutToken]
                          deriving (Show, Eq, Ord, Generic, Hashable)

-- | Internal helper function for converting a 'SubscriptionPosition' into a
-- JSON Object (not a JSON Value). Removes the need for partial pattern matching later.
subscriptionPositionToObject :: SubscriptionPosition -> Object
subscriptionPositionToObject = \case
    SubscriptionPositionBegin ->
      HashMap.fromList [("read_from", toJSON SubscriptionReadFromBegin)]
    SubscriptionPositionEnd ->
      HashMap.fromList [("read_from", toJSON SubscriptionReadFromEnd)]
    SubscriptionPositionCursors cursors ->
      HashMap.fromList [("read_from", toJSON SubscriptionReadFromCursors)
                       ,("cursors",   toJSON cursors)]

instance ToJSON SubscriptionPosition where
  toJSON = Object . subscriptionPositionToObject

instance FromJSON SubscriptionPosition where
  parseJSON = withObject "SubscriptionPosition" $ \obj -> do
    readFrom <- obj .: "read_from" >>= parseJSON
    case readFrom of
      SubscriptionReadFromBegin ->
        pure SubscriptionPositionBegin
      SubscriptionReadFromEnd ->
        pure SubscriptionPositionEnd
      SubscriptionReadFromCursors ->
        SubscriptionPositionCursors <$> obj .: "cursors"

-- | This type models the value describing the use case of a subscription.
-- In general this is an additional identifier used to differ subscriptions
-- having the same owning application and event types.
data ConsumerGroup = ConsumerGroup { unConsumerGroup :: Text }
  deriving (Eq, Ord, Show, Generic, Hashable)

instance ToJSON ConsumerGroup where
  toJSON = String . unConsumerGroup

instance FromJSON ConsumerGroup where
  parseJSON = parseString "ConsumerGroup" ConsumerGroup

-- | Type for a Subscription which has already been created.
--
-- When a subscription object is retrieved from Nakadi the following fields
-- are regarded as mandatory:
--
--   * @id@
--   * @owning_application@
--   * @event_types@
--   * @consumer_group@
--   * @created_at@
--   * @read_from@
--   * depending on @read_from@ also @cursors@.
data Subscription = Subscription
  { _id                   :: SubscriptionId
  , _owningApplication    :: ApplicationName
  , _eventTypes           :: [EventTypeName]
  , _consumerGroup        :: ConsumerGroup
  , _createdAt            :: Timestamp
  , _subscriptionPosition :: SubscriptionPosition
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON Subscription where
  parseJSON = withObject "Subscription" $ \obj ->
    Subscription <$> obj .: "id"
                 <*> obj .: "owning_application"
                 <*> obj .: "event_types"
                 <*> obj .: "consumer_group"
                 <*> obj .: "created_at"
                 <*> parseJSON (Object obj)

instance ToJSON Subscription where
  toJSON Subscription {..} =
      let obj = HashMap.fromList [ ("id",                 toJSON _id)
                                 , ("owning_application", toJSON _owningApplication)
                                 , ("event_types",        toJSON _eventTypes)
                                 , ("consumer_group",     toJSON _consumerGroup)
                                 , ("created_at",         toJSON _createdAt) ]
                <> subscriptionPositionToObject _subscriptionPosition
      in Object obj

-- | Type for a Subscription which is to be created.
--
-- When a subscription is to be created the following fields
-- are regarded as mandatory in the subscription object:
--
--   * @owning_application@
--   * @event_types@
--
-- The remaining fields are regarded as optional:
--
--   * @consumer_group@
--   * @read_from@
--   * depending on @read_from@ the field @cursors@ might
--     have to be present as well.
data SubscriptionRequest = SubscriptionRequest
  { _owningApplication    :: ApplicationName
  , _eventTypes           :: [EventTypeName]
  , _consumerGroup        :: Maybe ConsumerGroup
  , _subscriptionPosition :: Maybe SubscriptionPosition
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance FromJSON SubscriptionRequest where
  parseJSON = withObject "SubscriptionRequest" $ \obj ->
    SubscriptionRequest <$> obj .: "owning_application"
                        <*> obj .: "event_types"
                        <*> obj .:? "consumer_group"
                        <*> parseJSON (Object obj)

instance ToJSON SubscriptionRequest where
  toJSON SubscriptionRequest {..} =
    let obj = subscriptionPositionToObject (fromMaybe SubscriptionPositionEnd _subscriptionPosition)
              <> HashMap.fromList [ ("owning_application", toJSON _owningApplication)
                                  , ("event_types",        toJSON _eventTypes) ]
              <> case _consumerGroup of 
                  Just consumerGroup ->
                    HashMap.fromList [ ("consumer_group", toJSON consumerGroup) ]
                  Nothing ->
                    HashMap.empty
    in Object obj

-- | Type for publishing status.
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

-- | In case of failures during batch publishing, Nakadi returns
-- detailed information about which events failed to be published.
-- This per-event information is a batch item response.
data BatchItemResponse = BatchItemResponse
  { _eid              :: Maybe EventId
  , _publishingStatus :: PublishingStatus
  , _step             :: Maybe Step
  , _detail           :: Maybe Text
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions ''BatchItemResponse

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

newtype CursorCommitResults = CursorCommitResults
  { _items :: [CursorCommitResult]
  } deriving (Show, Eq, Ord)

deriveJSON nakadiJsonOptions ''CursorCommitResults

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
  { _links :: PaginationLinks
  , _items :: [EventTypeSchema]
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions {
  fieldLabelModifier = makeFieldRenamer [ ("_links", "_links")
                                        , ("_items", "items") ]
  }  ''EventTypeSchemasResponse


-- | SubscriptionsListResponse

data SubscriptionsListResponse = SubscriptionsListResponse
  { _links :: PaginationLinks
  , _items :: [Subscription]
  } deriving (Show, Eq, Ord, Generic, Hashable)

deriveJSON nakadiJsonOptions {
  fieldLabelModifier = makeFieldRenamer [ ("_links", "_links")
                                        , ("_items", "items") ]
  }  ''SubscriptionsListResponse

-- | Type for offset values.

newtype Offset = Offset
  { unOffset :: Int64 -- ^ Wrapped offset value
  } deriving (Show, Eq, Ord, Generic, Hashable)

-- | Type for limit values.

newtype Limit = Limit
  { unLimit :: Int64 -- ^ Wrapped limit value.
  } deriving (Show, Eq, Ord, Generic, Hashable)

-- | Type for partition states.

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

-- | Type for per-partition statistics.

data PartitionStat = PartitionStat
  { _partition          :: PartitionName
  , _state              :: PartitionState
  , _unconsumedEvents   :: Maybe Int64
  , _streamId           :: Maybe StreamId
  , _consumerLagSeconds :: Maybe Int64
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''PartitionStat

-- | Nakadi type @SubscriptionEventTypeStats@.

data SubscriptionEventTypeStats = SubscriptionEventTypeStats
  { _eventType  :: EventTypeName
  , _partitions :: [PartitionStat]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventTypeStats

-- | Type modelling per-subscription statistics. Objects of this type are returned by
-- requests to /subscriptions/SUBSCRIPTION-ID/stats.

newtype SubscriptionStats = SubscriptionStats
  { _items :: [SubscriptionEventTypeStats]
  } deriving (Show, Eq, Ord, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionStats

--  | Type for the category of an 'EventType'.

data EventTypeCategory = EventTypeCategoryUndefined
                       | EventTypeCategoryData
                       | EventTypeCategoryBusiness
                       deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON EventTypeCategory where
  toJSON category = String $ case category of
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

data PartitionStrategy = PartitionStrategyRandom
                       | PartitionStrategyUser
                       | PartitionStrategyHash
                       | PartitionStrategyCustom Text
                       deriving (Show, Eq, Ord, Generic, Hashable)

instance ToJSON PartitionStrategy where
  toJSON strategy = String $ case strategy of
    PartitionStrategyRandom      -> "random"
    PartitionStrategyUser        -> "user_defined"
    PartitionStrategyHash        -> "hash"
    PartitionStrategyCustom name -> name

instance FromJSON PartitionStrategy where
  parseJSON category = case category of
    "random"       -> return PartitionStrategyRandom
    "user_defined" -> return PartitionStrategyUser
    "hash"         -> return PartitionStrategyHash
    String other   -> return $ PartitionStrategyCustom other
    invalid        -> typeMismatch "PartitionStrategy" invalid

instance IsString PartitionStrategy where
  fromString = \case
    "random"       -> PartitionStrategyRandom
    "user_defined" -> PartitionStrategyUser
    "hash"         -> PartitionStrategyHash
    other          -> PartitionStrategyCustom (Text.pack other)

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

-- | Type for event type statistics.

data EventTypeStatistics = EventTypeStatistics
  { _messagesPerMinute :: Int64
  , _messageSize       :: Int64
  , _readParallelism   :: Int64
  , _writeParallelism  :: Int64
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''EventTypeStatistics

-- | Type for event type options.

data EventTypeOptions = EventTypeOptions
  { _retentionTime :: Int64
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''EventTypeOptions

-- | EventType

data EventType = EventType
  { _name                 :: EventTypeName
  , _owningApplication    :: Maybe ApplicationName
  , _category             :: Maybe EventTypeCategory
  , _enrichmentStrategies :: Maybe [EnrichmentStrategy]
  , _partitionStrategy    :: Maybe PartitionStrategy
  , _compatibilityMode    :: Maybe CompatibilityMode
  , _schema               :: EventTypeSchema
  , _partitionKeyFields   :: Maybe [PartitionKeyField]
  , _defaultStatistic     :: Maybe EventTypeStatistics
  , _options              :: Maybe EventTypeOptions
  } deriving (Show, Generic, Eq, Ord, Hashable)

deriveJSON nakadiJsonOptions ''EventType

-- | Type of published event metadata values.

data EventMetadata = EventMetadata
  { _eid        :: EventId
  , _occurredAt :: Timestamp
  , _parentEids :: Maybe [EventId]
  , _partition  :: Maybe PartitionName
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''EventMetadata

-- | Type of event metadata enriched by Nakadi

data EventMetadataEnriched = EventMetadataEnriched
  { _eid        :: EventId
  , _eventType  :: EventTypeName
  , _occurredAt :: Timestamp
  , _receivedAt :: Timestamp
  , _version    :: SchemaVersion
  , _parentEids :: Maybe [EventId]
  , _flowId     :: Maybe FlowId
  , _partition  :: Maybe PartitionName
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''EventMetadataEnriched

-- | SubscriptionEventStreamBatch

data SubscriptionEventStreamBatch a = SubscriptionEventStreamBatch
  { _cursor :: SubscriptionCursor -- ^ cursor for this subscription batch
  , _events :: Maybe (Vector a) -- ^ Events for this subscription batch
  } deriving (Show, Generic)

deriveJSON nakadiJsonOptions ''SubscriptionEventStreamBatch

-- | Type for "data_op" as contained in the DataChangeEvent.

data DataOp = DataOpCreation
            | DataOpUpdate
            | DataOpDeletion
            | DataOpSnapshot
            deriving (Show, Eq, Ord)

instance ToJSON DataOp where
  toJSON op = case op of
                DataOpCreation -> "C"
                DataOpUpdate   -> "U"
                DataOpDeletion -> "D"
                DataOpSnapshot -> "S"

instance FromJSON DataOp where
  parseJSON op = case op of
                   "C"     -> return DataOpCreation
                   "U"     -> return DataOpUpdate
                   "D"     -> return DataOpDeletion
                   "S"     -> return DataOpSnapshot
                   invalid -> typeMismatch "DataOp" invalid

-- | DataChangeEvent

data DataChangeEvent a = DataChangeEvent
  { _payload  :: a -- Cannot be named '_data', as this this would
                   -- cause the lense 'data' to be created, which is a
                   -- reserved keyword.
  , _metadata :: EventMetadata
  , _dataType :: Text
  , _dataOp   :: DataOp
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''DataChangeEvent

-- | A DataChangeEvent enriched by Nakadi
data DataChangeEventEnriched a = DataChangeEventEnriched
  { _payload  :: a -- Cannot be named '_data', as this this would
                   -- cause the lense 'data' to be created, which is a
                   -- reserved keyword.
  , _metadata :: EventMetadataEnriched
  , _dataType :: Text
  , _dataOp   :: DataOp
  } deriving (Eq, Show, Generic)

deriveJSON nakadiJsonOptions ''DataChangeEventEnriched

-- | Type modelling a "Business Event". Their JSON encodings are special since the payload
-- object is directly enriched with a @metadata@ field. "Data Change Events" on the other side
-- are JSON-encoded such that the complete event payload is contained in a seperate object field.
--
-- On the Haskell API side we split payload from meta data, which requires us to write custom
-- 'ToJSON' and 'FromJSON' implementations.

data BusinessEvent a = BusinessEvent
  { _payload  :: a             -- ^ Event Payload.
  , _metadata :: EventMetadata -- ^ Event Metadata.
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BusinessEvent a) where
  parseJSON = withObject "BusinessEvent" $ \obj ->
    BusinessEvent <$> parseJSON (Object obj) <*> obj .: "metadata"

instance ToJSON a => ToJSON (BusinessEvent a) where
  toJSON BusinessEvent {..} =
    case toJSON _payload of
      Object o ->
        -- If the JSON encoding of the event payload is an object, extend
        -- the object with metadata.
        Object (o <> metadata)
      _ ->
        -- Otherwise, produce an object that only contains metadata.
        Object metadata
    where metadata = HashMap.fromList [("metadata", toJSON _metadata)]

-- | Type modelling a Nakadi-enriched "Business Event". JSON encoding is basically the same as
-- for the non-enriched Business Events.
data BusinessEventEnriched a = BusinessEventEnriched
  { _payload  :: a -- Cannot be named '_data', as this this would
                   -- cause the lense 'data' to be created, which is a
                   -- reserved keyword.
  , _metadata :: EventMetadataEnriched
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BusinessEventEnriched a) where
  parseJSON = withObject "BusinessEventEnriched" $ \obj ->
    BusinessEventEnriched <$> parseJSON (Object obj) <*> obj .: "metadata"

instance ToJSON a => ToJSON (BusinessEventEnriched a) where
  toJSON BusinessEventEnriched {..} =
    case toJSON _payload of
      Object o ->
        -- If the JSON encoding of the event payload is an object, extend
        -- the object with metadata.
        Object (o <> metadata)
      _ ->
        -- Otherwise, produce an object that only contains metadata.
        Object metadata
    where metadata = HashMap.fromList [("metadata", toJSON _metadata)]
