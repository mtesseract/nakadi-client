{-|
Module      : Network.Nakadi.Internal.Committer
Description : Implementation of Cursor Committing Strategies
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This internal module implements cursor committing strategies to be
used by the subscription API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Committer where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Concurrent.Async.Timer       (Timer)
import qualified Control.Concurrent.Async.Timer       as Timer
import           Control.Concurrent.STM               (retry)
import           Control.Lens
import           Control.Monad.Logger
import qualified Data.HashMap.Strict                  as HashMap
import           UnliftIO.Async
import           UnliftIO.STM

import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Subscriptions.Cursors

type StagedCursorsWithCounter = StagedCursors Int

type StagedCursorsWithoutCounter = StagedCursors ()

cursorKey :: SubscriptionCursor -> (EventTypeName, PartitionName)
cursorKey cursor = (cursor^.L.eventType, cursor^.L.partition)

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

cursorBufferSize :: ConsumeParameters -> Int
cursorBufferSize consumeParams =
  case consumeParams^.L.maxUncommittedEvents of
    Nothing -> 1
    Just n  -> n
               & fromIntegral
               & (* safetyFactor)
               & round

  where safetyFactor     = 0.5

-- | Main function for the cursor committer thread. Logic depends on
-- the provided buffering strategy.
subscriptionCommitter
  :: forall b m
   . ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m )
  => CommitBufferingStrategy
  -> ConsumeParameters
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()

-- | Implementation for the 'CommitNoBuffer' strategy: We simply read
-- every cursor and commit it in order.
subscriptionCommitter CommitNoBuffer _consumeParams eventStream queue = loop
  where loop = do
          config <- nakadiAsk
          (_nEvents, cursor) <- liftIO . atomically . readTBQueue $ queue
          catchAny (subscriptionCursorCommit eventStream [cursor]) $ \ exn -> do
            nakadiLiftBase $
              case config^.L.logFunc of
                Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
                  "Failed to commit cursor " <> tshow cursor <> ": " <> tshow exn
                Nothing ->
                  pure ()
          loop

-- | Implementation of the 'CommitTimeBuffer' strategy: We use an
-- async timer for committing cursors at specified intervals.
--
-- The 'StagedCursor's in the 'CommitTimeBuffer' case carry no
-- additional information, just the subscription cursors.
subscriptionCommitter (CommitTimeBuffer millis) _consumeParams eventStream queue = do
  let timerConf = Timer.defaultConf
                  & Timer.setInitDelay (fromIntegral millis)
                  & Timer.setInterval  (fromIntegral millis)
  cursorsMap <- liftIO . atomically $
    newTVar (StagedCursors HashMap.empty :: StagedCursorsWithoutCounter)
  withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ \ timer -> forever $ do
      Timer.wait timer
      commitAllCursors eventStream cursorsMap

  where -- | The cursorsConsumer drains the cursors queue and adds each
        -- cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = forever . liftIO . atomically $ do
          (_, cursor) <- readTBQueue queue
          let key          = cursorKey cursor
              stagedCursor = StagedCursor cursor ()
          modifyTVar cursorsMap (L.cursorsMap %~ HashMap.insert key stagedCursor)

-- | Implementation of the 'CommitSmartBuffer' strategy: We use an
-- async timer for committing cursors at specified intervals, but if
-- the number of uncommitted events reaches some threshold before the
-- next scheduled commit, a commit is being done right away and the
-- timer is resetted.
--
-- The 'StagedCursor's in the 'CommitSmartBuffer' case carry an 'Int',
-- which is used for counting the number of events processed on the
-- respective partition since the last commit.
subscriptionCommitter CommitSmartBuffer consumeParams eventStream queue = do
  let millisDefault = 1000
      nMaxEvents    = cursorBufferSize consumeParams
      timerConf     = Timer.defaultConf
                      & Timer.setInitDelay (fromIntegral millisDefault)
                      & Timer.setInterval  (fromIntegral millisDefault)
  if nMaxEvents > 1
    then do cursorsMap <- liftIO . atomically $ newTVar (StagedCursors HashMap.empty)
            withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
              link asyncCursorConsumer
              Timer.withAsyncTimer timerConf $ cursorCommitter cursorsMap nMaxEvents
    else unbufferedCommitLoop eventStream queue

  where -- | The cursorsConsumer drains the cursors queue and adds
        -- each cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = forever . liftIO . atomically $ do
          (nEvents, cursor) <- readTBQueue queue
          let key          = cursorKey cursor
              stagedCursor = StagedCursor cursor nEvents
          modifyTVar cursorsMap $
            L.cursorsMap %~ (HashMap.insertWith updateCursor key stagedCursor)

        -- | Adds the old number of events to the new entry in the
        -- cursors map.
        updateCursor cursorNew cursorOld =
          cursorNew & L.enrichment %~ (+ (cursorOld^.L.enrichment))

        -- | Committer loop.
        cursorCommitter :: TVar StagedCursorsWithCounter -> Int -> Timer -> m ()
        cursorCommitter cursorsMap nMaxEvents timer = forever $  do
          race (Timer.wait timer) (maxEventsReached cursorsMap nMaxEvents) >>= \ case
            Left _ ->
              -- Timer has elapsed, simply commit all currently
              -- buffered cursors.
              commitAllCursors eventStream cursorsMap
            Right _ -> do
              -- Events processed since last cursor commit have
              -- crosses configured threshold for at least one
              -- partition. Commit cursors on all such partitions.
              Timer.reset timer
              commitAllCursors eventStream cursorsMap

        -- | Returns list of cursors that should be committed
        -- considering the number of events processed on the
        -- respective partition since the last commit. Blocks until at
        -- least one such cursor is found.
        maxEventsReached stagedCursorsTv nMaxEvents = liftIO . atomically $ do
          stagedCursors <- readTVar stagedCursorsTv
          let cursorsList   = HashMap.elems (stagedCursors^.L.cursorsMap)
              cursorsCommit = filter (shouldBeCommitted nMaxEvents) cursorsList
          if null cursorsCommit
            then retry
            else pure ()

        -- | Returns True if the provided staged cursor with should be
        -- committed. It is expected that the provided staged cursor
        -- carries an integral enrichment of the same type as
        -- @nMaxEvents@.
        shouldBeCommitted nMaxEvents stagedCursor =
          stagedCursor^.L.enrichment >= nMaxEvents

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

-- | Sink which can be used as sink for Conduits processing
-- subscriptions events. This sink takes care of committing events. It
-- can consume any values which contain Subscription Cursors.
subscriptionSink
  :: (MonadIO m, MonadNakadi b m)
  => SubscriptionEventStream
  -> ConduitM (SubscriptionEventStreamBatch a) void m ()
subscriptionSink eventStream = do
  config <- lift nakadiAsk
  awaitForever $ \ batch -> lift $ do
    let cursor  = batch^.L.cursor
    catchAny (commitOneCursor eventStream cursor) $ \ exn -> nakadiLiftBase $
      case config^.L.logFunc of
        Just logFunc -> logFunc "nakadi-client" LevelWarn $ toLogStr $
          "Failed to synchronously commit cursor: " <> tshow exn
        Nothing ->
          pure ()
