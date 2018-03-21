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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Subscriptions.Events
  ( subscriptionProcessConduit
  , subscriptionProcess
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit                             hiding (throwM)
import           Control.Lens
import           Data.Aeson
import           Network.HTTP.Client                 (responseBody)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses      as L
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
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     , FromJSON a )
  => Maybe ConsumeParameters                  -- ^ 'ConsumeParameters'
                                              -- to use
  -> SubscriptionId                           -- ^ Subscription to consume
  -> (SubscriptionEventStreamBatch a -> m ()) -- ^ Batch processor action
  -> m ()
subscriptionProcess maybeConsumeParameters subscriptionId processor =
  subscriptionProcessConduit maybeConsumeParameters subscriptionId conduit
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
     , batch ~ SubscriptionEventStreamBatch a )
  => Maybe ConsumeParameters   -- ^ 'ConsumeParameters' to use
  -> SubscriptionId            -- ^ Subscription to consume
  -> ConduitM batch batch m () -- ^ Conduit processor.
  -> m ()
subscriptionProcessConduit maybeConsumeParameters subscriptionId processor = do
  config <- nakadiAsk
  let consumeParams = fromMaybe defaultConsumeParameters maybeConsumeParameters
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
  httpJsonBodyStream ok200 [(status404, errorSubscriptionNotFound)]
    (includeFlowId config
     . setRequestPath path
     . setRequestQueryParameters queryParams) $
    subscriptionProcessHandler subscriptionId consumeParams processor

  where path = "/subscriptions/"
               <> subscriptionIdToByteString subscriptionId
               <> "/events"

-- | Derive a 'SubscriptionEventStream' from the provided
-- 'SubscriptionId' and Nakadi streaming response.
buildSubscriptionEventStream
  :: MonadThrow m
  => SubscriptionId
  -> Response a
  -> m SubscriptionEventStream
buildSubscriptionEventStream subscriptionId response =
  case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
    Just streamId ->
      pure SubscriptionEventStream
      { _streamId       = StreamId (decodeUtf8 streamId)
      , _subscriptionId = subscriptionId }
    Nothing ->
      throwM StreamIdMissing

-- | This function processes a subscription, taking care of
-- dispatching to worker threads and applying the configured
-- committing strategy.
subscriptionProcessHandler
  :: forall a b m batch.
     ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadResource m
     , MonadMask m
     , FromJSON a
     , batch ~ (SubscriptionEventStreamBatch a) )
  => SubscriptionId                         -- ^ Subscription ID required for committing.
  -> ConsumeParameters
  -> ConduitM batch batch m ()              -- ^ User provided Conduit for stream.
  -> Response (ConduitM () ByteString m ()) -- ^ Streaming response from Nakadi
  -> m ()
subscriptionProcessHandler subscriptionId consumeParams processor response = do
  config <- nakadiAsk
  let nWorkers = config^.L.worker.L.nThreads
  eventStream    <- buildSubscriptionEventStream subscriptionId response
  workerRegistry <- spawnWorkers subscriptionId eventStream consumeParams nWorkers processor
  race_ (workersWait workerRegistry) $ runConduit $
    responseBody response
    .| linesUnboundedAsciiC
    .| conduitDecode
    .| mapC (identity :: batch -> batch)
    .| workerDispatchSink workerRegistry
