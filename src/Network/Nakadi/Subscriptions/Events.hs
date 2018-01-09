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

module Network.Nakadi.Subscriptions.Events
  ( subscriptionSource
  , runSubscription
  , subscriptionSink
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Void
import           Network.HTTP.Simple
import           Network.HTTP.Types

import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Subscriptions.Cursors

-- | GET to @\/subscriptions\/SUBSCRIPTION\/events@. Creates a Conduit
-- Source producing events from a Subscription's event stream.
subscriptionSource ::
  (MonadNakadi b m, MonadResource m, MonadBaseControl IO m, FromJSON a
  , MonadSub b n, MonadIO n)
  => Maybe ConsumeParameters -- ^ Optional Consumption Parameters
  -> SubscriptionId          -- ^ Subscription ID
  -> m ( SubscriptionEventStream
       , ConduitM ()
         (SubscriptionEventStreamBatch a)
         (ReaderT (SubscriptionEventStreamContext b) n)
         () )                -- ^ Returns a Pair consisting of subscription
                             -- connection information ('SubscriptionEventStream')
                             -- and a Conduit source.
subscriptionSource maybeParams subscriptionId = do
  config <- nakadiAsk
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeParams
      queryParams = buildSubscriptionConsumeQueryParameters consumeParams

      addFlowId     = case consumeParams^.L.flowId of
                        Just flowId -> setRequestHeader "X-Flow-Id" [encodeUtf8 flowId]
                        Nothing     -> identity
  httpJsonBodyStream ok200 buildSubscriptionEventStream
    [(status404, errorSubscriptionNotFound)]
    (setRequestPath path . addFlowId . setRequestQueryParameters queryParams)

  where buildSubscriptionEventStream response =
          case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
            Just streamId ->
              Right SubscriptionEventStream
              { _streamId       = StreamId (decodeUtf8 streamId)
              , _subscriptionId = subscriptionId }
            Nothing ->
              Left "X-Nakadi-StreamId not found"

        path = "/subscriptions/"
               <> subscriptionIdToByteString subscriptionId
               <> "/events"

-- | Run a Subscription processing Conduit.
runSubscription ::
  (MonadNakadi b m, MonadBaseControl IO m, MonadResource m)
  => SubscriptionEventStream -- ^ Connection information for the Subscription
  -> ConduitM ()
              Void
              (ReaderT (SubscriptionEventStreamContext b) m)
              s              -- ^ Subscription Conduit to run
  -> m s                     -- ^ Result of the Conduit
runSubscription SubscriptionEventStream { .. } conduit = do
  config <- nakadiAsk
  let subscriptionStreamContext = SubscriptionEventStreamContext
                                  { _streamId       = _streamId
                                  , _subscriptionId = _subscriptionId
                                  , _ctxConfig      = config }
  runConduit . runReaderC subscriptionStreamContext $ conduit

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink ::
  (MonadNakadi b m, L.HasNakadiSubscriptionCursor a )
  => ConduitM a Void (ReaderT (SubscriptionEventStreamContext b) m) ()
subscriptionSink = awaitForever $ lift . subscriptionCommit . (: [])
