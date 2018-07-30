{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Examples.Subscription.Test
  ( testConsumption
  )
where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Logger
import qualified Network.Nakadi                as Nakadi
import           Network.Nakadi.Examples.Subscription.Process
import qualified Network.Nakadi.Lenses         as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty.HUnit
import           UnliftIO.Concurrent

genEvent :: MonadIO m => m (Nakadi.DataChangeEvent Foo)
genEvent = do
  now <- liftIO getCurrentTime
  eid <- Nakadi.EventId <$> genRandomUUID
  let event = Nakadi.DataChangeEvent
        { Nakadi._payload  = Foo "Hello!"
        , Nakadi._metadata = Nakadi.EventMetadata
          { Nakadi._eid        = eid
          , Nakadi._occurredAt = Nakadi.Timestamp now
          , Nakadi._parentEids = Nothing
          , Nakadi._partition  = Nothing
          }
        , Nakadi._dataType = "test.FOO"
        , Nakadi._dataOp   = Nakadi.DataOpUpdate
        }
  pure event

genEvents :: MonadIO m => m [Nakadi.DataChangeEvent Foo]
genEvents = replicateM 10 genEvent

testConsumption :: Nakadi.Config IO -> Assertion
testConsumption config = Nakadi.runNakadiT config $ do
  nakadiLogRef <- liftIO $ newIORef []
  bracket before after $ \subscriptionId -> do
    _ <- flip runLoggingT (logger nakadiLogRef) $ do
      events          <- genEvents
      publisherHandle <- async $ delayedPublish Nothing events
      link publisherHandle
      withAsync (dumpSubscription subscriptionId) $ \dumpHandle -> do
        link dumpHandle
        threadDelay (5 * 10 ^ 6) -- Give Nakadi some time to transmit the published events
    nakadiLog <- liftIO $ readIORef nakadiLogRef
    when (null nakadiLog) $ liftIO $ assertFailure
      "Subscription Consumption has logged no received batches"
 where
  before = do
    recreateEvent myEventType
    subscription <- Nakadi.subscriptionCreate Nakadi.SubscriptionRequest
      { _owningApplication    = "test-suite"
      , _eventTypes           = [myEventTypeName]
      , _consumerGroup        = Nothing -- ??
      , _subscriptionPosition = Just Nakadi.SubscriptionPositionEnd
      }
    pure $ subscription ^. L.id

  after subscriptionId = do
    Nakadi.subscriptionDelete subscriptionId
    Nakadi.eventTypeDelete myEventTypeName `catch` ignoreExnNotFound ()

  -- nEvents = 100

  logger nakadiLog loc logSource logLevel logStr = do
    let str = defaultLogStr loc logSource logLevel logStr
    liftIO $ modifyIORef nakadiLog (str :)
