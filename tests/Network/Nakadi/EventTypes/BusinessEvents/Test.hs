{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.EventTypes.BusinessEvents.Test where

import           ClassyPrelude
import           Data.Function                  ( (&) )
import           Network.Nakadi
import qualified Network.Nakadi.Lenses         as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Random
import           Control.Lens
import qualified Data.Set                      as Set
import           Data.Aeson

import           Conduit

data TestEventA = TestEventA
  { aInt :: Int
  , aString :: String
  } deriving (Eq, Generic, Show)

instance FromJSON TestEventA
instance ToJSON TestEventA

data EventSpec a b c = EventSpec
  { eventGenerator :: IO a
  , eventType :: EventType
  , eventPayload :: a -> c
  , eventEnrichedPayload :: b -> c
  }

eventTypeA :: EventType
eventTypeA = EventType
  { _name                 = "event-type-a"
  , _owningApplication    = Just "test-suite"
  , _category             = Just EventTypeCategoryBusiness
  , _enrichmentStrategies = Just [EnrichmentStrategyMetadata]
  , _partitionStrategy    = Nothing
  , _compatibilityMode    = Nothing
  , _schema               = EventTypeSchema
    { _version    = Nothing
    , _createdAt  = Nothing
    , _schemaType = SchemaTypeJson
    , _schema = "{\
                \ \"properties\": \
                \   { \
                \     \"aInt\": {\"type\": \"number\"}, \
                \     \"aString\": {\"type\": \"string\"} \
                \   }, \
                \ \"required\": [\"aInt\", \"aString\"] \
                \}"
    }
  , _partitionKeyFields   = Nothing
  , _defaultStatistic     = Nothing
  , _options              = Nothing
  }

eventSpecA :: EventSpec (BusinessEvent TestEventA) (BusinessEventEnriched TestEventA) TestEventA
eventSpecA = EventSpec genBusinessEventA eventTypeA (view L.payload) (view L.payload)

genBusinessEventA :: IO (BusinessEvent TestEventA)
genBusinessEventA = do
  payload   <- TestEventA <$> randomRIO (-100, 100) <*> pure "Hello"
  eid       <- EventId <$> randomIO
  timestamp <- Timestamp <$> getCurrentTime
  pure BusinessEvent
    { _payload  = payload
    , _metadata = EventMetadata
      { _eid                    = eid
      , _occurredAt             = timestamp
      , _parentEids             = Nothing
      , _partition              = Nothing
      , _partitionCompactionKey = Nothing
      }
    }

testBusinessEvents :: Config App -> TestTree
testBusinessEvents conf = testEvents conf "BusinessEvents" eventSpecA

testEvents
  :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b, Eq c, Show c)
  => Config App
  -> String
  -> EventSpec a b c
  -> TestTree
testEvents conf label eventSpec = testGroup
  label
  [ testCase "createAndDeleteEvent" (createAndDeleteEvent conf eventSpec)
  , testCase "publishAndConsume"    (publishAndConsume conf eventSpec)
  ]

createEventTypeFromSpec :: (MonadUnliftIO m, MonadNakadi base m) => EventSpec a b c -> m ()
createEventTypeFromSpec eventSpec = do
  subscriptionIds <- subscriptionsList Nothing (Just [eventSpec & eventType & _name])
    <&> map (view L.id)
  mapM_ subscriptionDelete subscriptionIds
  eventTypeDelete (eventSpec & eventType & _name) `catch` ignoreExnNotFound ()
  eventTypeCreate (eventType eventSpec)

deleteEventTypeFromSpec :: MonadNakadi base m => EventSpec a b c -> m ()
deleteEventTypeFromSpec eventSpec = eventTypeDelete (eventSpec & eventType & _name)

publishAndConsume
  :: forall a b c
   . (FromJSON a, ToJSON a, FromJSON b, ToJSON b, Eq c, Show c)
  => Config App
  -> EventSpec a b c
  -> Assertion
publishAndConsume conf' eventSpec =
  runApp
    . runNakadiT conf
    $ bracket_ (createEventTypeFromSpec eventSpec) (deleteEventTypeFromSpec eventSpec)
    $ withTemporarySubscription "test-suite"
                                "business-event-test"
                                (Set.fromList [eventSpec & eventType & _name])
                                SubscriptionPositionBegin
    $ \subscription -> do
        events :: [a] <- liftIO $ replicateM 10 (eventSpec & eventGenerator)
        eventsPublish (eventSpec & eventType & _name) events
        consumed :: [b] <-
          runConduitRes $ subscriptionSourceEvents (subscription ^. L.id) .| sinkList
        liftIO
          $   map (eventPayload eventSpec)         events
          @=? map (eventEnrichedPayload eventSpec) consumed
  where conf = conf' & setBatchFlushTimeout 1 & setStreamLimit 10

createAndDeleteEvent :: Config App -> EventSpec a b c -> Assertion
createAndDeleteEvent conf eventSpec =
  runApp
    . runNakadiT conf
    $ bracket_ (createEventTypeFromSpec eventSpec) (deleteEventTypeFromSpec eventSpec)
    $ pure ()
