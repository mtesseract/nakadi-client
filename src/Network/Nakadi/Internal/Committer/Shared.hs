{-|
Module      : Network.Nakadi.Internal.Committer.Shared
Description : Shared code for Committing Strategies
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Network.Nakadi.Internal.Committer.Shared where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import qualified Data.HashMap.Strict                  as HashMap

import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Internal.Logging
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Subscriptions.Cursors

import           UnliftIO.STM

-- | This function commits all cursors in the provided map of staged
-- cursors.
commitAllCursors
  :: (MonadNakadi b m, MonadIO m)
  => SubscriptionEventStream
  -> TVar (StagedCursors a)
  -> m ()
commitAllCursors eventStream stagedCursorsTv = do
  stagedCursors <- liftIO . atomically $
    swapTVar stagedCursorsTv (StagedCursors HashMap.empty)
  let cursors = map (view L.cursor) $ HashMap.elems (stagedCursors^.L.cursorsMap)
  forM_ cursors (commitOneCursor eventStream)

-- | This function takes care of committing a single cursor. Exceptions will be
-- catched and logged, but the failure will NOT be propagated. This means that
-- Nakadi itself is in control of disconnecting us.
commitOneCursor
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> SubscriptionCursor
  -> m ()
commitOneCursor eventStream cursor =
  catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn ->
    nakadiLogWarn [fmt|Failed to commit cursor ${tshow cursor}: $exn|]

-- | Naive cursor commit loop: We simply read every cursor and commit
-- it in order.
unbufferedCommitLoop
  :: (MonadNakadi b m, MonadIO m)
  => SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
unbufferedCommitLoop eventStream queue = forever $ do
  (_nEvents, cursor) <- liftIO . atomically . readTBQueue $ queue
  catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn ->
    nakadiLogWarn [fmt|Failed to commit cursor ${tshow cursor}: $exn|]

cursorKey :: SubscriptionCursor -> (EventTypeName, PartitionName)
cursorKey cursor = (cursor^.L.eventType, cursor^.L.partition)
