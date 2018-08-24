{-|
Module      : Network.Nakadi.Internal.Worker
Description : Implementation of Subscription Consumption Workers
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This internal module implements dispatching of batches to workers.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Nakadi.Internal.Worker
  ( WorkerRegistry
  , spawnWorkers
  , workerDispatchSink
  , workersWait
  )
where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import qualified Control.Monad.Trans.Resource  as Resource
import           Data.Aeson
import qualified Data.HashMap.Strict           as HashMap
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Vector                   as Vector
import           Network.Nakadi.EventTypes.Partitions
import           Network.Nakadi.Subscriptions.Subscription

import           Network.Nakadi.Internal.Committer
import qualified Network.Nakadi.Internal.Lenses
                                               as L
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Http   ( conduitDecodeBatchValue )
import           UnliftIO.Async
import           UnliftIO.STM

-- | Spawn a single asynchronous worker and return a 'Worker'. This worker value
-- contains a queue to which batches can be sent and they will be consumed and processed
-- by the asynchonous worker.
--
-- The worker resource allocation is protected by 'MonadResource'.
spawnWorker
  :: ( MonadNakadi b m
     , MonadResource m
     , MonadUnliftIO m
     , MonadMask m
     , MonadIO m
     , FromJSON a
     , batch ~ (SubscriptionEventStreamBatch a)
     )
  => SubscriptionEventStream   -- ^ Subscription to consume, required for committing
  -> ConduitM batch batch m () -- ^ Batch processor to run in the worker.
  -> m (Worker a)
spawnWorker eventStream processor = do
  u           <- askUnliftIO
  workerQueue <- atomically $ newTBQueue 1024
  let workerIO = unliftIO u (subscriptionWorker processor eventStream workerQueue)
  (_, workerAsync) <- Resource.allocate (async workerIO) cancel
  pure Worker {_queue = workerQueue, _async = workerAsync}

-- | Spawn multiple workers and return a 'WorkerRegistry'. It is guaranteed that the worker
-- registry contains at least one worker.
spawnWorkers
  :: ( MonadNakadi b m
     , MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     , FromJSON a
     , batch ~ SubscriptionEventStreamBatch a
     )
  => SubscriptionId
  -> SubscriptionEventStream
  -> Int
  -> ConduitM batch batch m ()
  -> m (WorkerRegistry a)
spawnWorkers subscriptionId eventStream nWorkers processor = do
  let workerIndices = fromMaybe (1 :| []) (NonEmpty.nonEmpty [1 .. nWorkers])
  workers           <- forM workerIndices (\_idx -> spawnWorker eventStream processor)
  partitionIndexMap <- retrievePartitionIndexMap subscriptionId
  pure WorkerRegistry {_workers = workers, _partitionIndexMap = partitionIndexMap}

-- | This function processes a subscription, taking care of applying
-- the configured committing strategy.
subscriptionWorker
  :: (MonadNakadi b m, MonadUnliftIO m, MonadResource m, MonadMask m, FromJSON a)
  => ConduitM (SubscriptionEventStreamBatch a) (SubscriptionEventStreamBatch a) m ()                   -- ^ User provided Conduit for stream.
  -> SubscriptionEventStream
  -> TBQueue (SubscriptionEventStreamBatch Value) -- ^ Streaming response from Nakadi
  -> m ()
subscriptionWorker processor eventStream queue = do
  config <- nakadiAsk
  -- This @producer@ is a Conduit producing subscription batches that have been successfully
  -- processed from the user provided callback @processor@.
  --
  -- What remains to be done with the batches produced by the producer is committing the
  -- corresponding cursors.
  let producer = repeatMC (atomically (readTBQueue queue)) .| conduitDecodeBatchValue .| processor
  -- For committing we distinguish between two distinct strategies: synchronous and
  -- asynchronous comitting.
  case config ^. L.commitStrategy of
    CommitSync ->
      -- Synchronous case: Simply use a Conduit sink that commits
      -- every cursor.
      --
      -- Run the Conduit, which reads batches from the queue, processes them
      -- and commits their cursors.
      runConduit $ producer .| subscriptionSink eventStream
    CommitAsync bufferingStrategy -> do
      -- Asynchronous case: Create a new queue and spawn a cursor
      -- committer thread depending on the configured commit buffering
      -- method. Then execute the provided Conduit processor with a
      -- sink that sends cursor information to the queue. The cursor
      -- committer thread reads from this queue and processes the
      -- cursors.
      --
      -- Run the Conduit, which reads batches from the queue, processes them
      -- and sends their cursors to the cursor committer thread implementing
      -- the actual cursor committing logic.
      commitQueue <- liftIO . atomically $ newTBQueue asyncCommitQueueBufferSize
      withAsync (subscriptionCommitter bufferingStrategy eventStream commitQueue) $ \asyncHandle ->
        do
          -- This makes sure that if the cursor committing thread dies because
          -- of an exception, this exception will be re-raised in the current thread.
          link asyncHandle
          runConduit $ producer .| mapM_C (sendToQueue commitQueue)
 where
  sendToQueue commitQueue batch = liftIO . atomically $ do
    let cursor  = batch ^. L.cursor
        events  = fromMaybe Vector.empty (batch ^. L.events)
        nEvents = length events
    writeTBQueue commitQueue (nEvents, cursor)

  asyncCommitQueueBufferSize = 1024

-- | Retrieve the 'PartitionIndexMap' for the given subscription. This map is used for mapping
-- per-event type partitions to worker indices. Given a pair consisting of an event type and
-- a partition ID, the worker index references the worker in the worker registry responsible
-- for processing batches originating from that partition.
retrievePartitionIndexMap :: MonadNakadi b m => SubscriptionId -> m PartitionIndexMap
retrievePartitionIndexMap subscriptionId = do
  eventTypes              <- (view L.eventTypes) <$> subscriptionGet subscriptionId
  eventTypesWithPartition <- concat <$> forM eventTypes extractPartitionsForEventType
  pure . HashMap.fromList $ zip eventTypesWithPartition [0 ..]
 where
  extractPartitionsForEventType eventType = do
    partitions <- map (view L.partition) <$> eventTypePartitions eventType
    pure (zip partitions (repeat eventType))

-- | Conduit sink which dispatches a batch to a worker contained in
-- the registry.
workerDispatchSink
  :: (MonadIO m) => WorkerRegistry a -> ConduitM (SubscriptionEventStreamBatch Value) Void m ()
workerDispatchSink registry = awaitForever $ \batch -> do
  let partition = batch ^. L.cursor . L.partition
      eventType = batch ^. L.cursor . L.eventType
      worker    = pickWorker registry eventType partition
  atomically $ writeTBQueue (_queue worker) batch

-- | Given a 'SubscriptionEventStreamBatch', produce the worker that
-- should handle the batch. The worker is found using the 'PartitionIndexMap'.
pickWorker :: WorkerRegistry a -> EventTypeName -> PartitionName -> Worker a
pickWorker registry eventType partition =
  let workers  = registry ^. L.workers
      nWorkers = NonEmpty.length workers
  in  case HashMap.lookup (partition, eventType) (registry ^. L.partitionIndexMap) of
        Nothing ->
          -- We failed to find an entry in the PartitionIndexMap for that partition.
          -- This should rarely happen.
          -- It could, for example, happen if the number of partitions of an event
          -- type belonging to the subscription to be consumed was increased after
          -- subscription consumption has started.
          --
          -- In the future, we could improve this fallback mechanism, if there is a
          -- need to do so. At the moment we simply use the first worker in case of
          -- lookup failures.
          NonEmpty.head workers
        Just idx ->
          -- Lookup successful. Truncate the resulting index using modulo in order to
          -- obtain a worker index and return the corresponding worker reference.
          workers NonEmpty.!! (idx `mod` nWorkers)

-- | Block as long no worker has finished. Workers are supposed to run forever, unless
-- cancelled. Therefore, using 'waitAnyCancel' essentially means that if some worker
-- fails due to an uncaught exception, then all other workers are cancelled as well.
workersWait :: MonadIO m => WorkerRegistry a -> m ()
workersWait registry = do
  let workerHandles = map _async $ NonEmpty.toList (registry ^. L.workers)
  void . waitAnyCancel $ workerHandles
