{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Examples.Subscription.Test
  ( testConsumption
  ) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Logger
import           Data.Maybe                                   (fromJust)
import qualified Network.Nakadi                               as Nakadi
import           Network.Nakadi.Examples.Subscription.Process
import qualified Network.Nakadi.Lenses                        as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty.HUnit
import UnliftIO.Concurrent (threadDelay)

genEvent :: MonadIO m => m (Nakadi.DataChangeEvent Foo)
genEvent = do
  now <- liftIO getCurrentTime
  eid <- Nakadi.EventId <$> genRandomUUID
  let event = Nakadi.DataChangeEvent
        { Nakadi._payload = Foo "Hello!"
        , Nakadi._metadata = Nakadi.EventMetadata
                             { Nakadi._eid = eid
                             , Nakadi._occurredAt = Nakadi.Timestamp now
                             , Nakadi._parentEids = Nothing
                             , Nakadi._partition = Nothing
                             }
        , Nakadi._dataType = "test.FOO"
        , Nakadi._dataOp = Nakadi.DataOpUpdate
        }
  pure event

genEvents :: MonadIO m => m [Nakadi.DataChangeEvent Foo]
genEvents =
  sequence (replicate 10 genEvent)

testConsumption :: Nakadi.Config IO -> Assertion
testConsumption config = Nakadi.runNakadiT config $ do
  nakadiLogRef <- liftIO $ newIORef []
  bracket before after $ \ subscriptionId -> do
    _ <- flip runLoggingT (logger nakadiLogRef) $ do
      events <- genEvents
      void . async $ delayedPublish Nothing events
      withAsync (dumpSubscription subscriptionId) $ \ _dumpHandle ->
        threadDelay (2 * 10^6) -- Give Nakadi some time to transmit the published events
    nakadiLog <- liftIO $ readIORef nakadiLogRef
    when (length nakadiLog == 0) $
      liftIO $ assertFailure "Subscription Consumption has logged no received batches"

  where before = do
          recreateEvent myEventTypeName myEventType
          subscription <- Nakadi.subscriptionCreate Nakadi.Subscription
            { _id = Nothing
            , _owningApplication = "test-suite"
            , _eventTypes = [myEventTypeName]
            , _consumerGroup = Nothing -- ??
            , _createdAt = Nothing
            , _readFrom = Just Nakadi.SubscriptionPositionEnd
            , _initialCursors = Nothing
            }
          pure . fromJust $ subscription^.L.id

        after subscriptionId = do
          Nakadi.subscriptionDelete subscriptionId
          Nakadi.eventTypeDelete myEventTypeName `catch` (ignoreExnNotFound ())

        -- nEvents = 100

        logger nakadiLog loc logSource logLevel logStr = do
          let str = defaultLogStr loc logSource logLevel logStr
          liftIO $ modifyIORef nakadiLog (str :)
