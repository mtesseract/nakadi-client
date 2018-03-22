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

module Network.Nakadi.Internal.Committer.Shared where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Monad.Logger
import qualified Data.HashMap.Strict                  as HashMap

import qualified Network.Nakadi.Internal.Lenses       as L
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
commitOneCursor eventStream cursor = do
  config <- nakadiAsk
  catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> nakadiLiftBase $
    case config^.L.logFunc of
      Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
        "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
      Nothing ->
        pure ()

-- | Implementation for the 'CommitNoBuffer' strategy: We simply read
-- every cursor and commit it in order.
unbufferedCommitLoop
  :: (MonadNakadi b m, MonadIO m)
  => SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
unbufferedCommitLoop eventStream queue = do
  config <- nakadiAsk
  forever $ do
    (_nEvents, cursor) <- liftIO . atomically . readTBQueue $ queue
    catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> do
      nakadiLiftBase $
        case config^.L.logFunc of
          Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
            "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
          Nothing ->
            pure ()

cursorKey :: SubscriptionCursor -> (EventTypeName, PartitionName)
cursorKey cursor = (cursor^.L.eventType, cursor^.L.partition)
