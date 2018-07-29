{-|
Module      : Network.Nakadi.Subscriptions.Events
Description : Implementation of Nakadi Subscription Events API
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements a high level interface for the
@\/subscriptions\/SUBSCRIPTIONS\/events@ API.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.Nakadi.Subscriptions.Events
  ( subscriptionProcessConduit
  , subscriptionProcess
  , subscriptionSource
  , subscriptionSourceEvents
  )
where

import           Network.Nakadi.Internal.Prelude

import           Control.Concurrent.STM.TBMQueue
                                                ( TBMQueue
                                                , newTBMQueue
                                                , writeTBMQueue
                                                , closeTBMQueue
                                                , readTBMQueue
                                                )
import           UnliftIO.STM                   ( atomically )

import           Conduit                 hiding ( throwM )
import           Control.Lens
import           Data.Aeson
import           Control.Monad.Trans.Resource   ( allocate )
import           Network.HTTP.Client            ( responseBody )
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Worker
import           UnliftIO.Async

-- For dispatching batches to workers, we maintain an integer-indexed
-- (non-empty) list of workers. Thus, we need have a way for mapping a
-- subscription batch, more precisely the cursors belonging to a
-- subscription batch, to some worker index. Mapping a cursor to an
-- Integer is sufficient, as we can simply take the reminder modulo
-- number of workers in our workers list.
--
-- Nakadi subscription cursors contain a partition reference and an
-- event type name — both given as strings. How do we derive an
-- integer from this data? The idea is to built a 'PartitionIndexMap'
-- upfrom, which allows us to establish this mapping. It is expected
-- that this map contains an entry for every valid @(PartitionName,
-- EventTypeName)@ combination for the subscription to be consumed.
-- If, for some reason, we receive cursors not contained in this map,
-- we map it to the zero index.

-- | Consumes the specified subscription using the commit strategy
-- contained in the configuration. Each consumed batch of subscription
-- events is provided to the provided batch processor action. If this
-- action throws an exception, subscription consumption will terminate.
subscriptionProcess
  :: (MonadNakadi b m, MonadUnliftIO m, MonadMask m, MonadResource m, FromJSON a)
  => SubscriptionId                           -- ^ Subscription to consume
  -> (SubscriptionEventStreamBatch a -> m ()) -- ^ Batch processor action
  -> m ()
subscriptionProcess subscriptionId processor = subscriptionProcessConduit subscriptionId conduit
  where conduit = iterMC processor

-- | Conduit based interface for subscription consumption. Consumes
-- the specified subscription using the commit strategy contained in
-- the configuration. Each consumed event batch is then processed by
-- the provided conduit. If an exception is thrown inside the conduit,
-- subscription consumption will terminate.
subscriptionProcessConduit
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadResource m
     , MonadMask m
     , FromJSON a
     , batch ~ SubscriptionEventStreamBatch a
     )
  => SubscriptionId            -- ^ Subscription to consume
  -> ConduitM batch batch m () -- ^ Conduit processor.
  -> m ()
subscriptionProcessConduit subscriptionId processor = do
  config <- nakadiAsk
  let queryParams = buildConsumeQueryParameters config
  httpJsonBodyStream
      ok200
      [(status404, errorSubscriptionNotFound)]
      (includeFlowId config . setRequestPath path . setRequestQueryParameters queryParams)
    $ subscriptionProcessHandler subscriptionId processor
  where path = "/subscriptions/" <> subscriptionIdToByteString subscriptionId <> "/events"

-- | Derive a 'SubscriptionEventStream' from the provided
-- 'SubscriptionId' and Nakadi streaming response.
buildSubscriptionEventStream
  :: MonadThrow m => SubscriptionId -> Response a -> m SubscriptionEventStream
buildSubscriptionEventStream subscriptionId response =
  case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
    Just streamId -> pure SubscriptionEventStream
      { _streamId       = StreamId (decodeUtf8 streamId)
      , _subscriptionId = subscriptionId
      }
    Nothing -> throwM StreamIdMissing

-- | This function processes a subscription, taking care of
-- dispatching to worker threads and applying the configured
-- committing strategy.
subscriptionProcessHandler
  :: forall a b m batch
   . ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadResource m
     , MonadMask m
     , FromJSON a
     , batch ~ (SubscriptionEventStreamBatch a)
     )
  => SubscriptionId                         -- ^ Subscription ID required for committing.
  -> ConduitM batch batch m ()              -- ^ User provided Conduit for stream.
  -> Response (ConduitM () ByteString m ()) -- ^ Streaming response from Nakadi
  -> m ()
subscriptionProcessHandler subscriptionId processor response = do
  config <- nakadiAsk
  let nWorkers = config ^. L.worker . L.nThreads
  eventStream    <- buildSubscriptionEventStream subscriptionId response
  workerRegistry <- spawnWorkers subscriptionId eventStream nWorkers processor
  race_ (workersWait workerRegistry)
    $  runConduit
    $  responseBody response
    .| linesUnboundedAsciiC
    .| conduitDecode
    .| mapC (identity :: batch -> batch)
    .| workerDispatchSink workerRegistry

-- | Experimental API.
--
-- Creates a Conduit source from a subscription ID. The source will produce subscription
-- event stream batches. Note the batches will be asynchronously committed irregardless of
-- any event processing logic. Use this function only if the guarantees provided by Nakadi
-- for event processing and cursor committing are not required or not desired.
subscriptionSource
  :: forall a b m
   . (MonadNakadi b m, MonadUnliftIO m, MonadMask m, MonadResource m, FromJSON a)
  => SubscriptionId                                    -- ^ Subscription to consume.
  -> ConduitM () (SubscriptionEventStreamBatch a) m () -- ^ Conduit source.
subscriptionSource subscriptionId = do
  UnliftIO {..}              <- lift askUnliftIO
  streamLimit                <- lift nakadiAsk <&> (fmap fromIntegral . view L.streamLimit)
  queue                      <- atomically $ newTBMQueue queueSize
  (_releaseKey, asyncHandle) <- allocate
    (unliftIO (async (subscriptionConsumer streamLimit queue)))
    cancel
  link asyncHandle
  drain queue
 where
  queueSize = 2048
  drain queue = atomically (readTBMQueue queue) >>= \case
    Just a  -> yield a >> drain queue
    Nothing -> pure ()

  subscriptionConsumer :: Maybe Int -> TBMQueue (SubscriptionEventStreamBatch a) -> m ()
  subscriptionConsumer maybeStreamLimit queue = go `finally` atomically (closeTBMQueue queue)
   where
    go = do
      subscriptionProcess subscriptionId (void . atomically . writeTBMQueue queue)
      -- We only reconnect automatically when no stream limit is set.
      -- This effectively means that we don't try to reach @streamLimit@ events exactly.
      -- We simply regard @streamLimit@ as an upper bound and in case Nakadi disconnects us
      -- earlier or the connection breaks, we produce less than @streamLimit@ events.
      when (isNothing maybeStreamLimit) go

-- | Experimental API.
--
-- Similar to `subscriptionSource`, but the created Conduit source provides events instead
-- of event batches.
subscriptionSourceEvents
  :: (MonadNakadi b m, MonadUnliftIO m, MonadMask m, MonadResource m, FromJSON a)
  => SubscriptionId     -- ^ Subscription to consume
  -> ConduitM () a m () -- ^ Conduit processor.
subscriptionSourceEvents subscriptionId =
  subscriptionSource subscriptionId .| concatMapC (\batch -> fromMaybe mempty (batch & _events))
