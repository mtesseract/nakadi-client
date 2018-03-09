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

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.Committer where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import qualified Control.Concurrent.Async.Timer       as Timer
import           Control.Concurrent.STM               (retry)
import           Control.Lens
import           Control.Monad.Logger
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           UnliftIO.Async
import           UnliftIO.STM

import           Network.Nakadi.Internal.Config
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Subscriptions.Cursors

-- | Main function for the cursor committer thread. Logic depends on
-- the provided buffering strategy.
subscriptionCommitter
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m )
  => CommitBufferingStrategy
  -> SubscriptionEventStream
  -> TBQueue (Int, SubscriptionCursor)
  -> m ()

-- | Implementation for the 'CommitNoBuffer' strategy: We simply read
-- every cursor and commit it in order.
subscriptionCommitter CommitNoBuffer eventStream queue = loop
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
subscriptionCommitter (CommitTimeBuffer millis) eventStream queue = do
  let timerConf = Timer.defaultConf
                  & Timer.setInitDelay (fromIntegral millis)
                  & Timer.setInterval  (fromIntegral millis)
  cursorsMap <- liftIO . atomically $ newTVar HashMap.empty
  withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ \ timer -> forever $ do
      Timer.wait timer
      commitAllCursors eventStream cursorsMap

  where -- | The cursorsConsumer drains the cursors queue and adds each
        -- cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = forever . liftIO . atomically $ do
          (_, cursor) <- readTBQueue queue
          modifyTVar cursorsMap (HashMap.insert (cursor^.L.partition) (0, cursor))

-- | Implementation of the 'CommitSmartBuffer' strategy: We use an
-- async timer for committing cursors at specified intervals, but if
-- the number of uncommitted events reaches some threshold before the
-- next scheduled commit, a commit is being done right away and the
-- timer is resetted.
subscriptionCommitter CommitSmartBuffer eventStream queue = do
  config <- nakadiAsk
  let millisDefault               = 1000
      nMaxUncommitedEventsDefault = 1000
      consumeParameters           = fromMaybe defaultConsumeParameters $
                                    config^.L.consumeParameters
      nMaxUncommittedEvents       = case consumeParameters^.L.maxUncommittedEvents of
                                      Just n  -> n
                                      Nothing -> nMaxUncommitedEventsDefault
      nMaxEvents                  = fromIntegral $ nMaxUncommittedEvents `div` 2
      timerConf                   = Timer.defaultConf
                                    & Timer.setInitDelay (fromIntegral millisDefault)
                                    & Timer.setInterval  (fromIntegral millisDefault)
  cursorsMap <- liftIO . atomically $
                newTVar (HashMap.empty :: HashMap PartitionName (Int, SubscriptionCursor))
  withAsync (cursorConsumer cursorsMap) $ \ asyncCursorConsumer -> do
    link asyncCursorConsumer
    Timer.withAsyncTimer timerConf $ cursorCommitter cursorsMap nMaxEvents

  where -- | The cursorsConsumer drains the cursors queue and adds
        -- each cursor to the provided cursorsMap.
        cursorConsumer cursorsMap = forever . liftIO . atomically $ do
          (nEvents, cursor) <- readTBQueue queue
          modifyTVar cursorsMap (HashMap.insertWith updateCursor (cursor^.L.partition) (nEvents, cursor))

        -- | Adds the old number of events to the new entry in the
        -- cursors map.
        updateCursor cursorNew _cursorOld @ (nEventsOld, _) =
          cursorNew & _1 %~ (+ nEventsOld)

        -- | Committer loop.
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
        maxEventsReached cursorsMap nMaxEvents = liftIO . atomically $ do
          cursorsList <- HashMap.toList <$> readTVar cursorsMap
          let cursorsCommit = filter (shouldBeCommitted nMaxEvents) cursorsList
          if null cursorsCommit
            then retry
            else pure ()

        -- | Returns True if the provided cursor should be committed.
        shouldBeCommitted nMaxEvents cursor = cursor^._2._1  >= nMaxEvents

-- | This function commits all cursors in the provided cursorsMap.
commitAllCursors
  :: (MonadNakadi b m, MonadIO m)
  => SubscriptionEventStream
  -> TVar (HashMap PartitionName (Int, SubscriptionCursor))
  -> m ()
commitAllCursors eventStream cursorsMap = do
  cursors <- liftIO . atomically $ swapTVar cursorsMap HashMap.empty
  forM_ cursors $ \ (_nEvents, cursor) -> commitOneCursor eventStream cursor


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
