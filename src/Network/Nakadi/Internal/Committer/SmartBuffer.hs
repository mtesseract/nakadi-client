{-|
Module      : Network.Nakadi.Internal.Committer.SmartBuffer
Description : Implementation of SmartBuffer based Cursor Committing Strategy
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Committer.SmartBuffer where

import           Network.Nakadi.Internal.Prelude

import           Control.Concurrent.Async.Timer ( Timer )
import qualified Control.Concurrent.Async.Timer
                                               as Timer
import qualified Data.HashMap.Strict           as HashMap

import           Control.Concurrent.STM         ( retry )
import           Control.Lens

import           Network.Nakadi.Internal.Committer.Shared
import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Types

import           UnliftIO.Async
import           UnliftIO.STM

-- | Implementation of the 'CommitSmartBuffer' strategy: We use an
-- async timer for committing cursors at specified intervals, but if
-- the number of uncommitted events reaches the threshold currently
-- specified by @maxUncommittedEvents@ divided by 2 (safety factor)
-- before the next scheduled commit, a commit is being done right away
-- and the timer is resetted.
--
-- The 'StagedCursor's in the 'CommitSmartBuffer' case carry an 'Int',
-- which is used for counting the number of events processed on the
-- respective partition since the last commit.
committerSmartBuffer
  :: forall b m
   . (MonadNakadi b m, MonadUnliftIO m, MonadMask m)
  => SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()
committerSmartBuffer eventStream queue = do
  nMaxEvents <- cursorBufferSize
  let millisDefault = 1000
      timerConf =
        Timer.defaultConf & Timer.setInitDelay (fromIntegral millisDefault) & Timer.setInterval
          (fromIntegral millisDefault)
  if nMaxEvents > 1
    then do
      cursorsMap <- liftIO . atomically $ newTVar HashMap.empty
      withAsync (cursorConsumer queue cursorsMap) $ \asyncCursorConsumer -> do
        link asyncCursorConsumer
        Timer.withAsyncTimer timerConf $ cursorCommitter eventStream cursorsMap nMaxEvents
    else unbufferedCommitLoop eventStream queue

-- | The cursorsConsumer drains the cursors queue and adds
-- each cursor to the provided cursorsMap.
cursorConsumer
  :: (MonadIO m)
  => TBQueue (Int, SubscriptionCursor)
  -> TVar (StagedCursorsMap SubscriptionCursorWithCounter)
  -> m ()
cursorConsumer queue cursorsMap = forever . liftIO . atomically $ do
  (nEvents, cursor) <- readTBQueue queue
  let key          = cursorKey cursor
      stagedCursor = SubscriptionCursorWithCounter cursor nEvents
  modifyTVar cursorsMap $ HashMap.insertWith updateCursor key stagedCursor

-- | Adds the old number of events to the new entry in the
-- cursors map.
updateCursor
  :: SubscriptionCursorWithCounter -> SubscriptionCursorWithCounter -> SubscriptionCursorWithCounter
updateCursor cursorNew cursorOld = cursorNew & L.nEvents %~ (+ (cursorOld ^. L.nEvents))

-- | Committer loop.
cursorCommitter
  :: (MonadNakadi b m, MonadUnliftIO m)
  => SubscriptionEventStream
  -> TVar (StagedCursorsMap SubscriptionCursorWithCounter)
  -> Int
  -> Timer
  -> m ()
cursorCommitter eventStream cursorsMap nMaxEvents timer =
  forever
    $   race (Timer.wait timer) (maxEventsReached cursorsMap nMaxEvents)
    >>= \case
          Left _ ->
            -- Timer has elapsed, simply commit all currently
            -- buffered cursors.
            commitAllCursors (view L.cursor) eventStream cursorsMap
          Right _ -> do
            -- Events processed since last cursor commit have
            -- crosses configured threshold for at least one
            -- partition. Commit cursors on all such partitions.
            Timer.reset timer
            commitAllCursors (view L.cursor) eventStream cursorsMap

-- | Returns list of cursors that should be committed
-- considering the number of events processed on the
-- respective partition since the last commit. Blocks until at
-- least one such cursor is found.
maxEventsReached
  :: MonadIO m => TVar (StagedCursorsMap SubscriptionCursorWithCounter) -> Int -> m ()
maxEventsReached stagedCursorsTv nMaxEvents = liftIO . atomically $ do
  stagedCursors <- readTVar stagedCursorsTv
  let cursorsList   = HashMap.elems stagedCursors
      cursorsCommit = filter (shouldBeCommitted nMaxEvents) cursorsList
  if null cursorsCommit then retry else pure ()

-- | Returns True if the provided staged cursor should be committed.
-- It is expected that the provided staged cursor carries an integral
-- enrichment of the same type as @nMaxEvents@.
shouldBeCommitted :: Int -> SubscriptionCursorWithCounter -> Bool
shouldBeCommitted nMaxEvents stagedCursor = stagedCursor ^. L.nEvents >= nMaxEvents

cursorBufferSize :: MonadNakadi b m => m Int
cursorBufferSize = do
  conf <- nakadiAsk
  pure $ case conf ^. L.maxUncommittedEvents of
    Nothing -> 1
    Just n  -> n & fromIntegral & (* safetyFactor) & round
  where safetyFactor = 0.5
