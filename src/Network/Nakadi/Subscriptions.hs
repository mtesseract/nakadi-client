{-|
Module      : Network.Nakadi.Subscriptions.Stats
Description : Implementation of Nakadi Subscription API
Copyright   : (c) Moritz Schulte 2017, 2018
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
  , subscriptionCreate
  , subscriptionsList'
  , subscriptionsSource
  , subscriptionsList
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import qualified Control.Exception.Safe                    as Safe
import           Control.Lens
import qualified Data.Text                                 as Text
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses            as L
import           Network.Nakadi.Internal.Util
import           Network.Nakadi.Subscriptions.Cursors
import           Network.Nakadi.Subscriptions.Events
import           Network.Nakadi.Subscriptions.Stats
import           Network.Nakadi.Subscriptions.Subscription

path :: ByteString
path = "/subscriptions"

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Low
-- level interface.
subscriptionCreate' ::
  MonadNakadi b m
  => Subscription
  -> m Subscription
subscriptionCreate' subscription =
  httpJsonBody status201 [(ok200, errorSubscriptionExistsAlready)]
  (setRequestMethod "POST"
   . setRequestPath path
   . setRequestBodyJSON subscription)

-- | @POST@ to @\/subscriptions@. Creates a new subscription. Does not
-- fail if the requested subscription does already exist.
subscriptionCreate ::
  (MonadNakadi b m, MonadCatch m)
  => Subscription
  -> m Subscription
subscriptionCreate subscription = do
  Safe.catchJust exceptionPredicate (subscriptionCreate' subscription) return

  where exceptionPredicate (SubscriptionExistsAlready s) = Just s
        exceptionPredicate _                             = Nothing

-- | @GET@ to @\/subscriptions@. Internal low-level interface.
subscriptionsGet ::
  MonadNakadi b m
  => [(ByteString, ByteString)]
  -> m SubscriptionsListResponse
subscriptionsGet queryParameters =
  httpJsonBody ok200 []
  (setRequestMethod "GET"
   . setRequestPath path
   . setRequestQueryParameters queryParameters)

buildQueryParameters :: Maybe ApplicationName
                     -> Maybe [EventTypeName]
                     -> Maybe Limit
                     -> Maybe Offset
                     -> [(ByteString, ByteString)]
buildQueryParameters maybeOwningApp maybeEventTypeNames maybeLimit maybeOffset =
  catMaybes $
  [ ("owning_application",) . encodeUtf8 . unApplicationName <$> maybeOwningApp
  , ("limit",) . encodeUtf8 . tshow <$> maybeLimit
  , ("offset",) . encodeUtf8 . tshow <$> maybeOffset ]
  ++ case maybeEventTypeNames of
       Just eventTypeNames -> map (Just . ("event_type",) . encodeUtf8 . unEventTypeName) eventTypeNames
       Nothing -> []

-- | @GET@ to @\/subscriptions@. Retrieves all subscriptions matching
-- the provided filter criteria. Low-level interface using pagination.
subscriptionsList' ::
  (MonadNakadi b m)
  => Maybe ApplicationName
  -> Maybe [EventTypeName]
  -> Maybe Limit
  -> Maybe Offset
  -> m SubscriptionsListResponse
subscriptionsList' maybeOwningApp maybeEventTypeNames maybeLimit maybeOffset = do
  subscriptionsGet queryParameters
  where queryParameters =
          buildQueryParameters maybeOwningApp maybeEventTypeNames maybeLimit maybeOffset

-- | @GET@ to @\/subscriptions@. Retrieves all subscriptions matching
-- the provided filter criteria. High-level Conduit interface.
subscriptionsSource :: ( MonadNakadi b m)
                    => Maybe ApplicationName
                    -> Maybe [EventTypeName]
                    -> m (ConduitM () [Subscription] m ())
subscriptionsSource maybeOwningApp maybeEventTypeNames =
  pure $ nextPage initialQueryParameters

  where nextPage queryParameters = do
          resp <- lift $ subscriptionsGet queryParameters
          yield (resp^.L.items)
          let maybeNextPath = Text.unpack . (view L.href) <$> (resp^.L.links.L.next)
          case maybeNextPath >>= extractQueryParametersFromPath  of
            Just nextQueryParameters -> do
              nextPage nextQueryParameters
            Nothing ->
              return ()

        initialQueryParameters =
          buildQueryParameters maybeOwningApp maybeEventTypeNames Nothing Nothing

-- | @GET@ to @\/subscriptions@. Retrieves all subscriptions matching
-- the provided filter criteria. High-level list interface.
subscriptionsList ::
  MonadNakadi b m
  => Maybe ApplicationName
  -> Maybe [EventTypeName]
  -> m [Subscription]
subscriptionsList maybeOwningApp maybeEventTypeNames = do
  source <- subscriptionsSource maybeOwningApp maybeEventTypeNames
  runConduit $ source .| concatC .| sinkList
