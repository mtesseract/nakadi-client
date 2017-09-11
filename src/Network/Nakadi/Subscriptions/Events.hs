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
  , subscriptionSourceR
  , runSubscription
  , runSubscriptionR
  , subscriptionSink
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Network.HTTP.Simple
import           Network.HTTP.Types

import           Data.Void
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Lenses       (HasSubscriptionCursor)
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Subscriptions.Cursors

-- | GET to @\/subscriptions\/SUBSCRIPTION\/events@. Creates a Conduit
-- Source producing events from a Subscription's event stream.
subscriptionSource ::
  (MonadNakadi m, MonadResource m, MonadBaseControl IO m, MonadMask m, FromJSON a)
  => Config                  -- ^ Configuration
  -> Maybe ConsumeParameters -- ^ Optional Consumption Parameters
  -> SubscriptionId          -- ^ Subscription ID
  -> m ( SubscriptionEventStream
       , ConduitM ()
         (SubscriptionEventStreamBatch a)
         (ReaderT SubscriptionEventStreamContext m)
         () )                -- ^ Returns a Pair consisting of subscription
                             -- connection information ('SubscriptionEventStream')
                             -- and a Conduit source.
subscriptionSource config maybeParams subscriptionId = do
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeParams
      queryParams = buildSubscriptionConsumeQueryParameters consumeParams

      addFlowId     = case _flowId consumeParams of
                        Just flowId -> setRequestHeader "X-Flow-Id" [encodeUtf8 flowId]
                        Nothing     -> identity
  httpJsonBodyStream config ok200 buildSubscriptionEventStream
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

-- | GET to @\/subscriptions\/SUBSCRIPTION\/events@. Creates a Conduit
-- Source producing events from a Subscription's event stream. Uses
-- the configuration from the environment.
subscriptionSourceR ::
  (MonadNakadiEnv r m, MonadResource m, MonadBaseControl IO m, MonadMask m, FromJSON a)
  => Maybe ConsumeParameters -- ^ Optional Consumption Parameters
  -> SubscriptionId          -- ^ Subscription ID
  -> m ( SubscriptionEventStream
       , ConduitM ()
         (SubscriptionEventStreamBatch a)
         (ReaderT SubscriptionEventStreamContext m)
         () )                -- ^ Returns a Pair consisting of subscription
                             -- connection information ('SubscriptionEventStream')
                             -- and a Conduit source.
subscriptionSourceR maybeParams subscriptionId = do
  config <- asks (view L.nakadiConfig)
  subscriptionSource config maybeParams subscriptionId

-- | Run a Subscription processing Conduit.
runSubscription ::
  (Monad m, MonadBaseControl IO m, MonadResource m)
  => Config                  -- ^ Configuration
  -> SubscriptionEventStream -- ^ Connection information for the Subscription
  -> ConduitM ()
              Void
              (ReaderT SubscriptionEventStreamContext m)
              r              -- ^ Subscription Conduit to run
  -> m r                     -- ^ Result of the Conduit
runSubscription config SubscriptionEventStream { .. } =
  let subscriptionStreamContext = SubscriptionEventStreamContext
                                  { _streamId       = _streamId
                                  , _subscriptionId = _subscriptionId
                                  , _config         = config }
  in runConduit . runReaderC subscriptionStreamContext

-- | Run a Subscription processing Conduit. Uses the configuration
-- contained in the environment.
runSubscriptionR ::
  (Monad m, MonadBaseControl IO m, MonadResource m, MonadReader r m, L.HasNakadiConfig r Config)
  => SubscriptionEventStream -- ^ Connection information for the Subscription
  -> ConduitM ()
              Void
              (ReaderT SubscriptionEventStreamContext m)
              r              -- ^ Subscription Conduit to run
  -> m r                     -- ^ Result of the Conduit
runSubscriptionR subscriptionEventStream conduit = do
  config <- asks (view L.nakadiConfig)
  runSubscription config subscriptionEventStream conduit

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink ::
  (MonadNakadi m, HasSubscriptionCursor a )
  => ConduitM a Void (ReaderT SubscriptionEventStreamContext m) ()
subscriptionSink = awaitForever $ lift . subscriptionCommit . (: [])
