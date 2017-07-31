-- | Subscription API

module Nakadi.Subscriptions.Subscription
  ( subscriptionGet
  , subscriptionDelete
  ) where

import           Nakadi.Internal.Prelude

import           Nakadi.Internal.Conversions
import           Nakadi.Internal.Http

path :: SubscriptionId -> ByteString
path subscriptionId = "/subscriptions" <> subscriptionIdToByteString subscriptionId

subscriptionGet :: MonadNakadi m
                => Config
                -> SubscriptionId
                -> m Subscription
subscriptionGet config subscriptionId =
  httpJsonBody config ok200 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

subscriptionDelete :: MonadNakadi m
                   => Config
                   -> SubscriptionId
                   -> m ()
subscriptionDelete config subscriptionId =
  httpJsonNoBody config status204 [(status404, errorSubscriptionNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path subscriptionId))
