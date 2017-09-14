module Network.Nakadi.EventTypes.Test where

import           Prelude

import           Control.Exception.Safe
import           Control.Monad
import           Network.Nakadi
import           Test.Tasty
import           Test.Tasty.HUnit

testEventTypes :: Config -> TestTree
testEventTypes conf = testGroup "EventTypes"
  [ testCase "EventTypesGet" (testEventTypesGet conf)
  , testCase "EventTypesDeleteCreateAndGet" (testEventTypesDeleteCreateGet conf)
  , testCase "EventTypePartitionsGet" (testEventTypePartitionsGet conf)
  ]

testEventTypesGet :: Config -> Assertion
testEventTypesGet conf =
  void $ eventTypesGet conf

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
  }

ignoreExnNotFound :: MonadThrow m => a -> NakadiException -> m a
ignoreExnNotFound a (EventTypeNotFound _) = return a
ignoreExnNotFound _ exn                   = throw exn

testEventTypesDeleteCreateGet :: Config -> Assertion
testEventTypesDeleteCreateGet conf = do
  eventTypeDelete conf myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate conf myEventType
  myEventTypes <- filterMyEvent <$> eventTypesGet conf
  length myEventTypes @=? 1
  eventTypeDelete conf myEventTypeName
  myEventTypes' <- filterMyEvent <$> eventTypesGet conf
  length myEventTypes' @=? 0

  where filterMyEvent = filter ((myEventTypeName ==) . _name)

testEventTypePartitionsGet :: Config -> Assertion
testEventTypePartitionsGet conf = do
  eventTypeDelete conf myEventTypeName `catch` (ignoreExnNotFound ())
  eventTypeCreate conf myEventType
  void $ eventTypePartitions conf myEventTypeName
