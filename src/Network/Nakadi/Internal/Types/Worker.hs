module Network.Nakadi.Internal.Types.Worker where

import           Network.Nakadi.Internal.Prelude

import           UnliftIO.Async
import           UnliftIO.STM
import           Data.Aeson                     ( Value )
import           Data.HashMap.Strict            ( HashMap )
import           Data.List.NonEmpty             ( NonEmpty )

import           Network.Nakadi.Internal.Types.Service

-- | Data type denoting an asynchronous worker.
data Worker a = Worker { _queue :: TBQueue (SubscriptionEventStreamBatch Value)
                       , _async :: Async ()
                       }

-- | Data type containing a non-empty list of worker references.
data WorkerRegistry a =
  WorkerRegistry { _workers           :: NonEmpty (Worker a)
                 , _partitionIndexMap :: PartitionIndexMap }

-- | Map used for mapping subscription batch cursors to worked
-- indices.
type PartitionIndexMap = HashMap (PartitionName, EventTypeName) Int
