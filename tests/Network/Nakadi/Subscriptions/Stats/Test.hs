{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- This test tests the subscription statistics API.

module Network.Nakadi.Subscriptions.Stats.Test where

import           ClassyPrelude

import           Control.Lens
import           Data.Aeson
import           Network.Nakadi
import qualified Network.Nakadi.Lenses         as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Concurrent
import           Control.Monad.Trans.Resource

testSubscriptionsStats :: Config App -> TestTree
testSubscriptionsStats conf = testGroup
  "Stats"
  [ testCase "SubscriptionStats/WithTimeLag" $ testSubscriptionStatsWithTimeLag conf
  , testCase "SubscriptionStats/WithoutTimeLag" $ testSubscriptionStatsWithoutTimeLag conf
  ]

produceSubscriptionStats :: Config App -> IO (Map EventTypeName [PartitionStat])
produceSubscriptionStats conf =
  runApp
    $ runNakadiT conf
    $ bracket before after
    $ \subscriptionId -> runResourceT $ do
    -- Note: Apparently we have to consume the subscription first in order to enable
    -- tracking of unconsumed events and time lag.
        void $ timeout (2 * 10 ^ (6 :: Int)) $ subscriptionProcess
          subscriptionId
          (\(_batch :: SubscriptionEventStreamBatch (DataChangeEvent Value)) -> pure ())
        events <- replicateM 10 (genMyDataChangeEventIdx 1)
        eventsPublish myEventTypeName events
        liftIO $ threadDelay (1 * 10 ^ (6 :: Int))
        subscriptionStats subscriptionId

testSubscriptionStatsWithoutTimeLag :: Config App -> Assertion
testSubscriptionStatsWithoutTimeLag conf = do
  stats <- produceSubscriptionStats (setShowTimeLag False conf)
  let (Just partitionStats) = lookup myEventTypeName stats
      noConsumerLagSeconds  = partitionStats & map (^. L.consumerLagSeconds) & filter isNothing
  liftIO $ length partitionStats @=? length noConsumerLagSeconds

testSubscriptionStatsWithTimeLag :: Config App -> Assertion
testSubscriptionStatsWithTimeLag conf = do
  stats <- produceSubscriptionStats (setShowTimeLag True conf)
  let (Just partitionStats) = lookup myEventTypeName stats
      consumerLagSeconds    = partitionStats & map (^. L.consumerLagSeconds) & filter isJust
  liftIO $ length partitionStats @=? length consumerLagSeconds

before :: (MonadUnliftIO m, MonadNakadi App m) => m SubscriptionId
before = do
  recreateEvent myEventType
  subscription <- subscriptionCreate SubscriptionRequest
    { _owningApplication    = "test-suite"
    , _eventTypes           = [myEventTypeName]
    , _consumerGroup        = Nothing
    , _subscriptionPosition = Just SubscriptionPositionBegin
    }
  pure $ subscription ^. L.id

after :: (MonadUnliftIO m, MonadNakadi App m) => SubscriptionId -> m ()
after subscriptionId = do
  subscriptionDelete subscriptionId
  eventTypeDelete myEventTypeName `catch` ignoreExnNotFound ()
