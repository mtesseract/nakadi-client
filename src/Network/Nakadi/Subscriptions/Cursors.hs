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
  , subscriptionCursorCommitR'
  , subscriptionCommit
  , subscriptionCursors
  , subscriptionCursorsR
  , subscriptionCursorsReset
  , subscriptionCursorsResetR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson

import qualified Control.Exception.Safe              as Safe
import           Control.Lens
import           Control.Monad.Reader
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
  => Config' b                -- ^ Configuration
  -> SubscriptionId           -- ^ Subsciption ID
  -> StreamId                 -- ^ Stream ID
  -> SubscriptionCursorCommit -- ^ Subscription Cursor to commit
  -> m ()
subscriptionCursorCommit' config subscriptionId streamId cursors =
  runNakadiT config $ subscriptionCursorCommitR' subscriptionId streamId cursors

-- | @POST@ to @\/subscriptions\/SUBSCRIPTION-ID\/cursors@. Commits
-- cursors using low level interface. Uses the configuration contained
-- in the environment.
subscriptionCursorCommitR' ::
  MonadNakadiEnv b m
  => SubscriptionId           -- ^ Subsciption ID
  -> StreamId                 -- ^ Stream ID
  -> SubscriptionCursorCommit -- ^ Subscription Cursor to commit
  -> m ()
subscriptionCursorCommitR' subscriptionId streamId cursors =
  httpJsonNoBody status204 [(ok200, errorCursorAlreadyCommitted)]
  (setRequestMethod "POST"
   . addRequestHeader "X-Nakadi-StreamId" (encodeUtf8 (unStreamId streamId))
   . setRequestBodyJSON cursors
   . setRequestPath (path subscriptionId))

-- | @POST@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Commits
-- cursors using high level interface.
subscriptionCommit ::
  (MonadNakadi b m, HasNakadiSubscriptionCursor a)
  => [a] -- ^ Values containing Subscription Cursors to commit
  -> ReaderT (SubscriptionEventStreamContext b) m ()
subscriptionCommit as = do
  SubscriptionEventStreamContext { .. } <- ask
  Safe.catchJust
    exceptionPredicate
    (lift (subscriptionCursorCommit' _ctxConfig _subscriptionId _streamId cursorsCommit))
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
  => Config' b              -- ^ Configuration
  -> SubscriptionId         -- ^ Subscription ID
  -> m [SubscriptionCursor] -- ^ Subscription Cursors for the specified Subscription
subscriptionCursors config subscriptionId =
  runNakadiT config $ subscriptionCursorsR subscriptionId

-- | @GET@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Retrieves
-- subscriptions cursors, using the configuration from the
-- environment.
subscriptionCursorsR ::
  MonadNakadiEnv b m
  => SubscriptionId         -- ^ Subscription ID
  -> m [SubscriptionCursor] -- ^ Subscription Cursors for the specified Subscription
subscriptionCursorsR subscriptionId =
  httpJsonBody ok200 []
  (setRequestMethod "GET" . setRequestPath (path subscriptionId))

-- | @PATCH@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Resets
-- subscriptions cursors.
subscriptionCursorsReset ::
  MonadNakadi b m
  => Config' b                        -- ^ Configuration
  -> SubscriptionId                   -- ^ Subscription ID
  -> [SubscriptionCursorWithoutToken] -- ^ Subscription Cursors to reset
  -> m ()
subscriptionCursorsReset config subscriptionId cursors =
  runNakadiT config $ subscriptionCursorsResetR subscriptionId cursors

-- | @PATCH@ to @\/subscriptions\/SUBSCRIPTION\/cursors@. Resets
-- subscriptions cursors, using the configuration from the
-- environment.
subscriptionCursorsResetR ::
  MonadNakadiEnv b m
  => SubscriptionId                   -- ^ Subscription ID
  -> [SubscriptionCursorWithoutToken] -- ^ Subscription Cursors to reset
  -> m ()
subscriptionCursorsResetR subscriptionId cursors =
  httpJsonNoBody status204 [ (status404, errorSubscriptionNotFound)
                           , (status409, errorCursorResetInProgress) ]
  (setRequestMethod "PATCH"
   . setRequestPath (path subscriptionId)
   . setRequestBodyJSON (Object (HashMap.fromList [("items", toJSON cursors)])))
