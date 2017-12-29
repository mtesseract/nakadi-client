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

import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http

path :: SubscriptionId -> ByteString
path subscriptionId =
  "/subscriptions/"
  <> subscriptionIdToByteString subscriptionId

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION@. Looks up subscription
-- information for a subscription ID.
subscriptionGet ::
  MonadNakadi b m
  => Config' b      -- ^ Configuration
  -> SubscriptionId -- ^ Subscription ID
  -> m Subscription -- ^ Resulting Subscription Information
subscriptionGet config subscriptionId =
  httpJsonBody config ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION@. Looks up subscription
-- information for a subscription ID. Uses configuration from the
-- environment.
subscriptionGetR ::
  MonadNakadiEnv b m
  => SubscriptionId -- ^ Subscription ID
  -> m Subscription -- ^ Resulting Subscription Information
subscriptionGetR subscriptionId = do
  config <- nakadiAsk
  subscriptionGet config subscriptionId

-- | @DELETE@ to @\/subscriptions\/SUBSCRIPTION@. Deletes a
-- subscription by subscription ID.
subscriptionDelete ::
  MonadNakadi b m
  => Config' b      -- ^ Configuration
  -> SubscriptionId -- ^ ID of the Subcription to delete
  -> m ()
subscriptionDelete config subscriptionId =
  httpJsonNoBody config status204 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path subscriptionId))

-- | @DELETE@ to @\/subscriptions\/SUBSCRIPTION@. Deletes a
-- subscription by subscription ID. Uses configuration contained in
-- the environment.
subscriptionDeleteR ::
  MonadNakadiEnv b m
  => SubscriptionId -- ^ ID of the Subcription to delete
  -> m ()
subscriptionDeleteR subscriptionId = do
  config <- nakadiAsk
  subscriptionDelete config subscriptionId
