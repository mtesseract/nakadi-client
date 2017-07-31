{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Nakadi.Subscriptions.Events
  ( subscriptionSource
  , runSubscription
  ) where

import           Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Network.HTTP.Simple
import           Network.HTTP.Types

import           Data.Void
import           Nakadi.Internal.Config
import           Nakadi.Internal.Conversions
import           Nakadi.Internal.Http
import           Nakadi.Internal.Types
import qualified Nakadi.Internal.Lenses      as L

-- | GET to /subscriptions/ID/events
subscriptionSource :: (MonadNakadi m, FromJSON a, MonadResource m, MonadBaseControl IO m, MonadMask m)
                   => Config
                   -> Maybe ConsumeParameters
                   -> SubscriptionId
                   -> m ( SubscriptionEventStream
                        , ConduitM ()
                                   (SubscriptionEventStreamBatch a)
                                   (ReaderT SubscriptionEventStreamContext m)
                                   ())
subscriptionSource config maybeParams subscriptionId = do
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeParams
      queryParams = buildSubscriptionConsumeQueryParameters consumeParams

      addFlowId     = case _flowId consumeParams of
                        Just flowId -> setRequestHeader "X-Flow-Id" [encodeUtf8 flowId]
                        Nothing     -> identity
  httpJsonBodyStream config ok200 buildSubscriptionEventStream [(status404, errorSubscriptionNotFound)]
    (setRequestPath path . addFlowId . setRequestQueryParameters queryParams)

  where buildSubscriptionEventStream response =
          case listToMaybe (getResponseHeader "X-Nakadi-StreamId" response) of
            Just streamId ->
              Right SubscriptionEventStream { _streamId       = StreamId (decodeUtf8 streamId)
                                            , _subscriptionId = subscriptionId }
            Nothing ->
              Left "X-Nakadi-StreamId not found"

        path = "/subscriptions/" <> subscriptionIdToByteString subscriptionId <> "/events"

runSubscription :: (Monad m, MonadBaseControl IO m, MonadResource m)
                => Config
                -> SubscriptionEventStream
                -> ConduitM () Void (ReaderT SubscriptionEventStreamContext m) r
                -> m r
runSubscription config SubscriptionEventStream { .. } =
  let subscriptionStreamContext = SubscriptionEventStreamContext { _streamId       = _streamId
                                                                 , _subscriptionId = _subscriptionId
                                                                 , _config         = config }
  in runConduit . runReaderC subscriptionStreamContext
