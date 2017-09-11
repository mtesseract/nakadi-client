{-|
Module      : Network.Nakadi.Subscriptions.Stats
Description : Implementation of Nakadi Subscription API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/subscriptions\/SUBSCRIPTIONS@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Subscriptions.Subscription
  ( subscriptionGet
  , subscriptionGetR
  , subscriptionDelete
  , subscriptionDeleteR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses      as L

path :: SubscriptionId -> ByteString
path subscriptionId =
  "/subscriptions"
  <> subscriptionIdToByteString subscriptionId

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION@. Looks up subscription
-- information for a subscription ID.
subscriptionGet ::
  MonadNakadi m
  => Config         -- ^ Configuration
  -> SubscriptionId -- ^ Subscription ID
  -> m Subscription -- ^ Resulting Subscription Information
subscriptionGet config subscriptionId =
  httpJsonBody config ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION@. Looks up subscription
-- information for a subscription ID. Uses configuration from the
-- environment.
subscriptionGetR ::
  MonadNakadiEnv r m
  => SubscriptionId -- ^ Subscription ID
  -> m Subscription -- ^ Resulting Subscription Information
subscriptionGetR subscriptionId = do
  config <- asks (view L.nakadiConfig)
  subscriptionGet config subscriptionId

-- | @DELETE@ to @\/subscriptions\/SUBSCRIPTION@. Deletes a
-- subscription by subscription ID.
subscriptionDelete ::
  MonadNakadi m
  => Config         -- ^ Configuration
  -> SubscriptionId -- ^ ID of the Subcription to delete
  -> m ()
subscriptionDelete config subscriptionId =
  httpJsonNoBody config status204 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path subscriptionId))

-- | @DELETE@ to @\/subscriptions\/SUBSCRIPTION@. Deletes a
-- subscription by subscription ID. Uses configuration contained in
-- the environment.
subscriptionDeleteR ::
  MonadNakadiEnv r m
  => SubscriptionId -- ^ ID of the Subcription to delete
  -> m ()
subscriptionDeleteR subscriptionId = do
  config <- asks (view L.nakadiConfig)
  subscriptionDelete config subscriptionId
