{-|
Module      : Network.Nakadi.Subscriptions.Stats
Description : Implementation of Nakadi Subscription API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/subscriptions@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Network.Nakadi.Subscriptions
  ( module Network.Nakadi.Subscriptions.Cursors
  , module Network.Nakadi.Subscriptions.Events
  , module Network.Nakadi.Subscriptions.Stats
  , module Network.Nakadi.Subscriptions.Subscription
  , subscriptionCreate'
  , subscriptionCreateR'
  , subscriptionCreate
  , subscriptionCreateR
  , subscriptionsList
  , subscriptionsListR
  ) where

import           Network.Nakadi.Internal.Prelude

import qualified Control.Exception.Safe                    as Safe
import           Control.Lens
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses            as L
import           Network.Nakadi.Subscriptions.Cursors
import           Network.Nakadi.Subscriptions.Events
import           Network.Nakadi.Subscriptions.Stats
import           Network.Nakadi.Subscriptions.Subscription

path :: ByteString
path = "/subscriptions"

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Low
-- level interface.
subscriptionCreate' :: MonadNakadi m
                    => Config
                    -> Subscription
                    -> m Subscription
subscriptionCreate' config subscription =
  httpJsonBody config status201 [(ok200, errorSubscriptionExistsAlready)]
  (setRequestMethod "POST" . setRequestPath path . setRequestBodyJSON subscription)

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Low
-- level interface. Retrieves configuration from the environment.
subscriptionCreateR' ::
  MonadNakadiEnv r m
  => Subscription
  -> m Subscription
subscriptionCreateR' subscription = do
  config <- asks (view L.nakadiConfig)
  subscriptionCreate config subscription

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Does not
-- fail if the requested subscription does already exist.
subscriptionCreate :: MonadNakadi m
                   => Config
                   -> Subscription
                   -> m Subscription
subscriptionCreate config subscription =
  Safe.catchJust exceptionPredicate (subscriptionCreate' config subscription) return

  where exceptionPredicate (SubscriptionExistsAlready s) = Just s
        exceptionPredicate _                             = Nothing

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Does not
-- fail if the requested subscription does already exist. Retrieves
-- configuration from the environment.
subscriptionCreateR ::
  MonadNakadiEnv r m
  => Subscription
  -> m Subscription
subscriptionCreateR subscription = do
  config <- asks (view L.nakadiConfig)
  subscriptionCreate config subscription

-- | @GET@ to @\/subscriptions@. Retrieves all subscriptions matching
-- the provided filter criteria.
subscriptionsList :: MonadNakadi m
                  => Config
                  -> Maybe ApplicationName
                  -> Maybe [EventTypeName]
                  -> m SubscriptionsListResponse
subscriptionsList config owningApp eventTypeNames =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath path . setRequestQueryParameters queryParameters)

  where queryParameters =
          let owningAppBS      = encodeUtf8 . unApplicationName <$> owningApp
              eventTypeNamesBS = maybe [] (map (encodeUtf8 . unEventTypeName)) eventTypeNames
          in catMaybes $
             (("owning_application",) <$> owningAppBS)
             : map (Just . ("event_type",)) eventTypeNamesBS

-- | @GET@ to @\/subscriptions@. Retrieves all subscriptions matching
-- the provided filter criteria. Uses configuration contained in the
-- environment.
subscriptionsListR ::
  MonadNakadiEnv r m
  => Maybe ApplicationName
  -> Maybe [EventTypeName]
  -> m SubscriptionsListResponse
subscriptionsListR owningApp eventTypeNames = do
  config <- asks (view L.nakadiConfig)
  subscriptionsList config owningApp eventTypeNames
