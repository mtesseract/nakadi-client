{-|
Module      : Network.Nakadi.Subscriptions.Stats
Description : Implementation of Nakadi Subscription Stats API
Copyright   : (c) Moritz Schulte 2017
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
  , subscriptionStatsR'
  , subscriptionStats
  , subscriptionStatsR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import qualified Data.Map.Strict                     as Map
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses      as L

path :: SubscriptionId -> ByteString
path subscriptionId =
  "/subscriptions/"
  <> subscriptionIdToByteString subscriptionId
  <> "/cursors"

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Low level
-- interface for Subscriptions Statistics retrieval.
subscriptionStats' ::
  MonadNakadi b m
  => Config' b                          -- ^ Configuration
  -> SubscriptionId                     -- ^ Subscription ID
  -> m SubscriptionEventTypeStatsResult -- ^ Subscription Statistics
subscriptionStats' config subscriptionId =
  runNakadiT config $ subscriptionStatsR' subscriptionId

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Low level
-- interface for Subscriptions Statistics retrieval. Obtains
-- configuration from environment.
subscriptionStatsR' ::
  MonadNakadiEnv b m
  => SubscriptionId                     -- ^ Subscription ID
  -> m SubscriptionEventTypeStatsResult -- ^ Subscription Statistics
subscriptionStatsR' subscriptionId =
  httpJsonBody ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. High level
-- interface for Subscription Statistics retrieval.
subscriptionStats ::
  MonadNakadi b m
  => Config' b                             -- ^  Configuration
  -> SubscriptionId                        -- ^ Subscription ID
  -> m (Map EventTypeName [PartitionStat]) -- ^ Subscription
                                           -- Statistics as a 'Map'.
subscriptionStats config subscriptionId =
  runNakadiT config $ subscriptionStatsR subscriptionId

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. High level
-- interface for Subscription Statistics retrieval, obtains
-- configuration from environment..
subscriptionStatsR ::
  MonadNakadiEnv b m
  => SubscriptionId                        -- ^ Subscription ID
  -> m (Map EventTypeName [PartitionStat]) -- ^ Subscription
                                           -- Statistics as a 'Map'.
subscriptionStatsR subscriptionId = do
  items <- subscriptionStatsR' subscriptionId <&> view L.items
  return . Map.fromList . map (\SubscriptionEventTypeStats { .. } -> (_eventType, _partitions)) $ items
