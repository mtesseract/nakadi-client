-- | Subscription Statistics

{-# LANGUAGE RecordWildCards #-}

module Nakadi.Subscriptions.Stats
  ( subscriptionStats'
  , subscriptionStats
  ) where

import           Nakadi.Internal.Prelude

import           Control.Lens
import qualified Data.Map.Strict             as Map
import           Nakadi.Internal.Conversions
import           Nakadi.Internal.Http
import qualified Nakadi.Internal.Lenses      as L

path :: SubscriptionId -> ByteString
path subscriptionId = "/subscriptions/" <> subscriptionIdToByteString subscriptionId <> "/cursors"

subscriptionStats' :: MonadNakadi m
                   => Config
                   -> SubscriptionId
                   -> m SubscriptionEventTypeStatsResult
subscriptionStats' config subscriptionId =
  httpJsonBody config ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

subscriptionStats :: MonadNakadi m
                  => Config
                  -> SubscriptionId
                  -> m (Map EventTypeName [PartitionStat])
subscriptionStats config subscriptionId = do
  items <- subscriptionStats' config subscriptionId <&> view L.items
  return . Map.fromList . map (\SubscriptionEventTypeStats { .. } -> (_eventType, _partitions)) $ items
