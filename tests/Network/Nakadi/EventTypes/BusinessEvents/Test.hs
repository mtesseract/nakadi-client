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
import           Data.Aeson
import           UnliftIO.Async                 ( async )
import           Control.Monad.Catch            ( MonadMask )

import           Conduit

data TestEventA = TestEventA
  { aInt :: Int
  , aString :: String
  } deriving (Eq, Generic, Show)

instance FromJSON TestEventA
instance ToJSON TestEventA

data EventSpec a = EventSpec
  { eventGenerator :: IO a
  , eventType :: EventType
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

eventSpecA :: EventSpec (BusinessEvent TestEventA)
eventSpecA = EventSpec genBusinessEventA eventTypeA

genBusinessEventA :: IO (BusinessEvent TestEventA)
genBusinessEventA = do
  payload   <- TestEventA <$> randomRIO (-100, 100) <*> pure "Hello"
  eid       <- EventId <$> randomIO
  timestamp <- Timestamp <$> getCurrentTime
  pure BusinessEvent
    { _payload  = payload
    , _metadata = EventMetadata
      { _eid        = eid
      , _occurredAt = timestamp
      , _parentEids = Nothing
      , _partition  = Nothing
      }
    }

testBusinessEvents :: Config App -> TestTree
testBusinessEvents conf =
  testEvents conf "BusinessEvents" eventSpecA (view L.payload)

testEvents
  :: (FromJSON a, ToJSON a, Eq a, Show a, Show b, Eq b)
  => Config App
  -> String
  -> EventSpec a
  -> (a -> b)
  -> TestTree
testEvents conf label eventSpec f = testGroup
  label
  [ testCase "createAndDeleteEvent" (createAndDeleteEvent conf eventSpec)
  , testCase "publishAndConsume"    (publishAndConsume conf eventSpec f)
  ]

createEventTypeFromSpec
  :: (MonadUnliftIO m, MonadNakadi b m) => EventSpec a -> m ()
createEventTypeFromSpec eventSpec = do
  subscriptionIds <-
    subscriptionsList Nothing (Just [eventSpec & eventType & _name])
      <&> mapMaybe (view L.id)
  mapM_ subscriptionDelete subscriptionIds
  eventTypeDelete (eventSpec & eventType & _name) `catch` (ignoreExnNotFound ())
  eventTypeCreate (eventType eventSpec)

deleteEventTypeFromSpec :: MonadNakadi b m => EventSpec a -> m ()
deleteEventTypeFromSpec eventSpec =
  eventTypeDelete (eventSpec & eventType & _name)

publishAndConsume
  :: forall a b
   . (FromJSON a, ToJSON a, Eq a, Show a, Eq b, Show b)
  => Config App
  -> EventSpec a
  -> (a -> b)
  -> Assertion
publishAndConsume conf eventSpec f =
  runApp
    . runNakadiT conf
    $ bracket_ (createEventTypeFromSpec eventSpec)
               (deleteEventTypeFromSpec eventSpec)
    $ bracket (createSubscription eventSpec) subscriptionDelete
    $ \subscriptionId -> do
        events :: [a] <- liftIO $ replicateM 10 (eventSpec & eventGenerator)
        eventsPublish (eventSpec & eventType & _name) events
        consumed :: [a] <-
          runConduit
          $  subscriptionSourceEvents subscriptionId
          .| takeC 10
          .| sinkList
        liftIO $ map f events @=? map f consumed

subscriptionSourceEvents
  :: (MonadNakadi b m, MonadUnliftIO m, MonadMask m, FromJSON a)
  => SubscriptionId            -- ^ Subscription to consume
  -> ConduitM () a m () -- ^ Conduit processor.
subscriptionSourceEvents subscriptionId = subscriptionSource subscriptionId
  .| concatMapC (\batch -> fromMaybe mempty (batch & _events))

subscriptionSource
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , FromJSON a
     , batch ~ SubscriptionEventStreamBatch a
     )
  => SubscriptionId            -- ^ Subscription to consume
  -> ConduitM () batch m () -- ^ Conduit processor.
subscriptionSource subscriptionId = do
  queue <- atomically newTQueue
  void
    . lift
    . async
    $ subscriptionProcess (Just consumeParams) subscriptionId
    $ \batch -> void . atomically $ writeTQueue queue batch
  forever $ do
    batch <- atomically $ readTQueue queue
    yield batch
  where consumeParams = defaultConsumeParameters & setBatchFlushTimeout 1

createSubscription :: MonadNakadi b m => EventSpec a -> m SubscriptionId
createSubscription eventSpec = do
  subscription <- subscriptionCreate Subscription
    { _id                = Nothing
    , _owningApplication = "test-suite"
    , _eventTypes        = [eventSpec & eventType & _name]
    , _consumerGroup     = Nothing
    , _createdAt         = Nothing
    , _readFrom          = Just SubscriptionPositionBegin
    , _initialCursors    = Nothing
    }
  let (Just subscriptionId) = subscription & _id
  pure subscriptionId

createAndDeleteEvent :: Config App -> EventSpec a -> Assertion
createAndDeleteEvent conf eventSpec =
  runApp
    . runNakadiT conf
    $ bracket_ (createEventTypeFromSpec eventSpec)
               (deleteEventTypeFromSpec eventSpec)
    $ pure ()

