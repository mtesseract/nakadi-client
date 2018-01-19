{-|
Module      : Network.Nakadi.Subscriptions.Cursors
Description : Implementation of Nakadi Subscription Cursors API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/subscriptions\/SUBSCRIPTIONS\/cursors@
API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.Nakadi.Subscriptions.Cursors
  ( subscriptionCursorCommit'
  , subscriptionCursorCommit
  , subscriptionCursors
  , subscriptionCursorsReset
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson

import qualified Control.Exception.Safe              as Safe
import           Control.Lens
import qualified Data.HashMap.Lazy                   as HashMap
import           Network.Nakadi.Internal.Conversions
import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Lenses      (HasNakadiSubscriptionCursor)
import qualified Network.Nakadi.Internal.Lenses      as L

path :: SubscriptionId -> ByteString
path subscriptionId =
  "/subscriptions/"
  <> subscriptionIdToByteString subscriptionId
  <> "/cursors"

-- | @POST@ to @\/subscriptions\/SUBSCRIPTION-ID\/cursors@. Commits
-- cursors using low level interface.
subscriptionCursorCommit' ::
  MonadNakadi b m
  => SubscriptionId           -- ^ Subsciption ID
  -> StreamId                 -- ^ Stream ID
  -> SubscriptionCursorCommit -- ^ Subscription Cursor to commit
  -> m ()
subscriptionCursorCommit' subscriptionId streamId cursors =
  httpJsonNoBody status204 [(ok200, errorCursorAlreadyCommitted)]
  (setRequestMethod "POST"
   . addRequestHeader "X-Nakadi-StreamId" (encodeUtf8 (unStreamId streamId))
   . setRequestBodyJSON cursors
   . setRequestPath (path subscriptionId))

-- | @POST@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Commits
-- cursors using high level interface.
subscriptionCursorCommit ::
  (MonadNakadi b m, MonadCatch m, HasNakadiSubscriptionCursor a)
  => SubscriptionEventStream
  -> [a] -- ^ Values containing Subscription Cursors to commit
  -> m ()
subscriptionCursorCommit SubscriptionEventStream { .. } as  = do
  Safe.catchJust
    exceptionPredicate
    (subscriptionCursorCommit' _subscriptionId _streamId cursorsCommit)
    (const (return ()))

  where exceptionPredicate = \case
          CursorAlreadyCommitted _ -> Just ()
          _ -> Nothing

        cursors = map (^. L.subscriptionCursor) as
        cursorsCommit = SubscriptionCursorCommit cursors

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Retrieves
-- subscriptions cursors.
subscriptionCursors ::
  MonadNakadi b m
  => SubscriptionId         -- ^ Subscription ID
  -> m [SubscriptionCursor] -- ^ Subscription Cursors for the specified Subscription
subscriptionCursors subscriptionId =
  httpJsonBody ok200 []
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @PATCH@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Resets
-- subscriptions cursors.
subscriptionCursorsReset ::
  MonadNakadi b m
  => SubscriptionId                   -- ^ Subscription ID
  -> [SubscriptionCursorWithoutToken] -- ^ Subscription Cursors to reset
  -> m ()
subscriptionCursorsReset subscriptionId cursors =
  httpJsonNoBody status204 [ (status404, errorSubscriptionNotFound)
                           , (status409, errorCursorResetInProgress) ]
  (setRequestMethod "PATCH"
   . setRequestPath (path subscriptionId)
   . setRequestBodyJSON (Object (HashMap.fromList [("items", toJSON cursors)])))
