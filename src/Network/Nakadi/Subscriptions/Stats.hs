{-|
Module      : Network.Nakadi.Subscriptions.Stats
Description : Implementation of Nakadi Subscription Stats API
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/subscriptions\/SUBSCRIPTIONS\/stats@
API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.Nakadi.Subscriptions.Stats
  ( subscriptionStats'
  , subscriptionStats
  )
where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Data.Aeson
import qualified Data.Map.Strict               as Map
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Util

path :: SubscriptionId -> ByteString
path subscriptionId =
  "/subscriptions/" <> subscriptionIdToByteString subscriptionId <> "/stats"

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/stats@. Low level
-- interface for Subscriptions Statistics retrieval.
subscriptionStats'
  :: MonadNakadi b m
  => SubscriptionId      -- ^ Subscription ID
  -> Bool                -- ^ Whether to show time lag.
  -> m SubscriptionStats -- ^ Subscription Statistics
subscriptionStats' subscriptionId showTimeLag = httpJsonBody
  ok200
  [(status404, errorSubscriptionNotFound)]
  ( setRequestMethod "GET"
  . setRequestPath (path subscriptionId)
  . setRequestQueryParameters queryParameters
  )
  where queryParameters = [("show_time_lag", encodeStrict (Bool showTimeLag))]

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/stats@. High level
-- interface for Subscription Statistics retrieval.
subscriptionStats
  :: MonadNakadi b m
  => SubscriptionId                        -- ^ Subscription ID
  -> [SubscriptionStatsParameter]          -- ^ Optional parameters, currently only `ShowTimeLag` is supported.
  -> m (Map EventTypeName [PartitionStat]) -- ^ Subscription
                                           -- Statistics as a 'Map'.
subscriptionStats subscriptionId parameters = do
  items <- subscriptionStats' subscriptionId showTimeLag <&> view L.items
  return
    . Map.fromList
    . map (\SubscriptionEventTypeStats {..} -> (_eventType, _partitions))
    $ items
  where showTimeLag = ShowTimeLag `elem` parameters
