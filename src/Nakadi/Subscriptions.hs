-- | Subscription API

{-# LANGUAGE TupleSections #-}

module Nakadi.Subscriptions
  ( module Nakadi.Subscriptions.Cursors
  , module Nakadi.Subscriptions.Events
  , module Nakadi.Subscriptions.Stats
  , module Nakadi.Subscriptions.Subscription
  , subscriptionCreate
  , subscriptionsList
  ) where

import           Nakadi.Internal.Prelude

import qualified Control.Exception.Safe            as Safe
import           Nakadi.Internal.Http
import           Nakadi.Subscriptions.Cursors
import           Nakadi.Subscriptions.Events
import           Nakadi.Subscriptions.Stats
import           Nakadi.Subscriptions.Subscription

path :: ByteString
path = "/subscriptions"

subscriptionCreate' :: MonadNakadi m
                    => Config
                    -> Subscription
                    -> m Subscription
subscriptionCreate' config subscription =
  httpJsonBody config status201 [(ok200, errorSubscriptionExistsAlready)]
  (setRequestMethod "POST" . setRequestPath path . setRequestBodyJSON subscription)

subscriptionCreate :: MonadNakadi m
                   => Config
                   -> Subscription
                   -> m Subscription
subscriptionCreate config subscription =
  Safe.catchJust exceptionPredicate (subscriptionCreate' config subscription) return

  where exceptionPredicate (SubscriptionExistsAlready s) = Just s
        exceptionPredicate _                             = Nothing

subscriptionsList :: MonadNakadi m
                  => Config
                  -> Maybe ApplicationName
                  -> Maybe [EventTypeName]
                  -> m [Subscription]
subscriptionsList config owningApp eventTypeNames =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath path . setRequestQueryParameters queryParameters)

  where queryParameters =
          let owningAppBS      = encodeUtf8 . unApplicationName <$> owningApp
              eventTypeNamesBS = maybe [] (map (encodeUtf8 . unEventTypeName)) eventTypeNames
          in catMaybes $
             (("owning_application",) <$> owningAppBS)
             : map (Just . ("event_type",)) eventTypeNamesBS
