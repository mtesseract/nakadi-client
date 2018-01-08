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
  , subscriptionDelete
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
  MonadNakadiEnv b m
  => SubscriptionId -- ^ Subscription ID
  -> m Subscription -- ^ Resulting Subscription Information
subscriptionGet subscriptionId =
  httpJsonBody ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @DELETE@ to @\/subscriptions\/SUBSCRIPTION@. Deletes a
-- subscription by subscription ID.
subscriptionDelete ::
  MonadNakadiEnv b m
  => SubscriptionId -- ^ ID of the Subcription to delete
  -> m ()
subscriptionDelete subscriptionId =
  httpJsonNoBody status204 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path subscriptionId))
