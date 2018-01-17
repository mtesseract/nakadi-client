{-|
Module      : Network.Nakadi.Subscriptions.Events
Description : Implementation of Nakadi Subscription Events API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements a high level interface for the
@\/subscriptions\/SUBSCRIPTIONS\/events@ API.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Subscriptions.Events
  ( subscriptionProcessConduit
  , subscriptionProcess
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit                              hiding (throwM)
import           Control.Lens
import           Data.Aeson
import           Data.Void
import           Network.HTTP.Client                  (responseBody)
import           Network.HTTP.Simple
import           Network.HTTP.Types

import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Subscriptions.Cursors

subscriptionProcess
  :: ( MonadNakadi b m
     , FromJSON a
     , MonadNakadiHttpStream b m
     , NakadiHttpStreamConstraint m
     )
  => Maybe ConsumeParameters
  -> SubscriptionId
  -> (SubscriptionEventStreamBatch a -> m ())
  -> m ()
subscriptionProcess maybeConsumeParameters subscriptionId processor =
  subscriptionProcessConduit maybeConsumeParameters subscriptionId conduit
  where conduit = iterMC processor

subscriptionProcessConduit
  :: ( MonadNakadi b m
     , FromJSON a
     , MonadNakadiHttpStream b m
     , NakadiHttpStreamConstraint m
     , L.HasNakadiSubscriptionCursor c
     )
  => Maybe ConsumeParameters
  -> SubscriptionId
  -> ConduitM (SubscriptionEventStreamBatch a) c m ()
  -> m ()
subscriptionProcessConduit maybeConsumeParameters subscriptionId processor = do
  config <- nakadiAsk
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeConsumeParameters
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
      addFlowId     = case consumeParams^.L.flowId of
                        Just flowId -> setRequestHeader "X-Flow-Id" [encodeUtf8 flowId]
                        Nothing     -> identity
  httpJsonBodyStream ok200 [(status404, errorSubscriptionNotFound)]
    (setRequestPath path . addFlowId . setRequestQueryParameters queryParams) $
    handler config


  where buildSubscriptionEventStream response =
          case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
            Just streamId ->
              pure SubscriptionEventStream
              { _streamId       = StreamId (decodeUtf8 streamId)
              , _subscriptionId = subscriptionId }
            Nothing ->
              throwM StreamIdMissing

        path = "/subscriptions/"
               <> subscriptionIdToByteString subscriptionId
               <> "/events"

        handler config response = do
          eventStream <- buildSubscriptionEventStream response
          runConduit $
            responseBody response
            .| linesUnboundedAsciiC
            .| conduitDecode config
            .| processor
            .| subscriptionSink eventStream


-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink ::
  (MonadNakadi b m, L.HasNakadiSubscriptionCursor a)
  => SubscriptionEventStream
  -> ConduitM a Void m ()
subscriptionSink eventStream =
  awaitForever $ lift . subscriptionCursorCommit eventStream . (: [])
