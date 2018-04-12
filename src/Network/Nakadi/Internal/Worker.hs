{-|
Module      : Network.Nakadi.Internal.Worker
Description : Implementation of Subscription Consumption Workers
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This internal module implements cursor committing strategies to be
used by the subscription API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Nakadi.Internal.Worker
  ( WorkerRegistry
  , spawnWorkers
  , workerDispatchSink
  , workersWait
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import qualified Control.Monad.Trans.Resource              as Resource
import           Data.Aeson
import qualified Data.HashMap.Strict                       as HashMap
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.List.NonEmpty                        as NonEmpty
import qualified Data.Vector                               as Vector
import           Network.Nakadi.EventTypes.Partitions
import           Network.Nakadi.Subscriptions.Subscription

import           Network.Nakadi.Internal.Committer
import qualified Network.Nakadi.Internal.Lenses            as L
import           Network.Nakadi.Internal.Logging
import           Network.Nakadi.Internal.Types
import           UnliftIO.Async
import           UnliftIO.STM

-- | Spawn a single worker.
spawnWorker
  :: ( MonadNakadi b m
     , MonadResource m
     , MonadUnliftIO m
     , MonadMask m
     , MonadIO m
     , FromJSON a
     , batch ~ (SubscriptionEventStreamBatch a) )
  => SubscriptionEventStream
  -> ConsumeParameters
  -> ConduitM batch batch m ()
  -> m (Worker a)
spawnWorker eventStream consumeParams processor = do
  u <- askUnliftIO
  workerQueue <- atomically $ newTBQueue 1024
  let workerIO = unliftIO u (subscriptionWorker processor eventStream consumeParams workerQueue)
  (_, workerAsync) <- Resource.allocate (async workerIO) cancel
  pure Worker { _queue = workerQueue
              , _async = workerAsync }

-- | Spawn multiple workers and return a 'WorkerRegistry'.
spawnWorkers
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     , FromJSON a
     , batch ~ SubscriptionEventStreamBatch a )
  => SubscriptionId
  -> SubscriptionEventStream
  -> ConsumeParameters
  -> Int
  -> ConduitM batch batch m ()
  -> m (WorkerRegistry a)
spawnWorkers subscriptionId eventStream consumeParams nWorkers processor = do
  let workerIndices = fromMaybe (1 :| []) (NonEmpty.nonEmpty [1 .. nWorkers])
  workers           <- forM workerIndices (\_idx -> spawnWorker eventStream consumeParams processor)
  nakadiLogDebug [fmt|Number of workers spawned for subscription consumption: $nWorkers|]
  partitionIndexMap <- retrievePartitionIndexMap subscriptionId
  pure WorkerRegistry { _workers = workers
                      , _partitionIndexMap = partitionIndexMap }

-- | This function processes a subscription, taking care of applying
-- the configured committing strategy.
subscriptionWorker
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadResource m
     , MonadMask m
     , FromJSON a
     , batch ~ (SubscriptionEventStreamBatch a) )
  => ConduitM batch batch m ()              -- ^ User provided Conduit for stream.
  -> SubscriptionEventStream
  -> ConsumeParameters
  -> TBQueue batch                          -- ^ Streaming response from Nakadi
  -> m ()
subscriptionWorker processor eventStream consumeParams queue = do
  config      <- nakadiAsk
  let producer = repeatMC (atomically (readTBQueue queue)) .| processor
  case config^.L.commitStrategy of
    CommitSync ->
      -- Synchronous case: Simply use a Conduit sink that commits
      -- every cursor.
      runConduit $ producer .| subscriptionSink eventStream
    CommitAsync bufferingStrategy -> do
      -- Asynchronous case: Create a new queue and spawn a cursor
      -- committer thread depending on the configured commit buffering
      -- method. Then execute the provided Conduit processor with a
      -- sink that sends cursor information to the queue. The cursor
      -- committer thread reads from this queue and processes the
      -- cursors.
      commitQueue <- liftIO . atomically $ newTBQueue 1024
      withAsync (subscriptionCommitter bufferingStrategy consumeParams eventStream commitQueue) $
        \ asyncHandle -> do
          link asyncHandle
          runConduit $ producer .| mapM_C (sendToQueue commitQueue)

  where sendToQueue commitQueue batch = liftIO . atomically $ do
          let cursor  = batch^.L.cursor
              events  = fromMaybe Vector.empty (batch^.L.events)
              nEvents = length events
          writeTBQueue commitQueue (nEvents, cursor)

-- | Retrieve the 'PartitionIndexMap' for the given subscription.
retrievePartitionIndexMap
  :: MonadNakadi b m
  => SubscriptionId
  -> m PartitionIndexMap
retrievePartitionIndexMap subscriptionId = do
  eventTypes <- (view L.eventTypes) <$> subscriptionGet subscriptionId
  eventTypesWithPartition <- concat <$> forM eventTypes extractPartitionsForEventType
  pure . HashMap.fromList $ zip eventTypesWithPartition [0..]

  where extractPartitionsForEventType eventType = do
          partitions <- map (view L.partition) <$> eventTypePartitions eventType
          pure (zip partitions (repeat eventType))

-- | Conduit sink which dispatches a batch to a worker contained in
-- the registry.
workerDispatchSink
  :: (MonadIO m)
  => WorkerRegistry a
  -> ConduitM (SubscriptionEventStreamBatch a) Void m ()
workerDispatchSink registry = awaitForever $ \ batch -> do
  let  partition = batch^.L.cursor^.L.partition
       eventType = batch^.L.cursor^.L.eventType
       worker    = pickWorker registry eventType partition
  atomically $ writeTBQueue (_queue worker) batch

-- | Given a 'SubscriptionEventStreamBatch', produce the worker that
-- should handle the batch.
pickWorker
  :: WorkerRegistry a
  -> EventTypeName
  -> PartitionName
  -> Worker a
pickWorker registry eventType partition =
  let workers   = registry^.L.workers
      nWorkers  = NonEmpty.length workers
  in case HashMap.lookup (partition, eventType) (registry^.L.partitionIndexMap) of
       Nothing  -> NonEmpty.head workers
       Just idx -> workers NonEmpty.!! (idx `mod` nWorkers)

-- | Block as long no worker finishes.
workersWait :: MonadIO m => WorkerRegistry a -> m ()
workersWait registry = do
  let workerHandles = map _async $ NonEmpty.toList (registry^.L.workers)
  void . waitAny $ workerHandles
