module Network.Nakadi.EventTypes.Partitions
  ( eventTypePartitions
  , eventTypePartition
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> Maybe PartitionName -> ByteString
path eventTypeName maybePartitionName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/partitions"
  <> (case maybePartitionName of
        Just partitionName -> "/" <> encodeUtf8 (unPartitionName partitionName)
        Nothing            -> "")

-- | GET to /event-types/NAME/partitions
eventTypePartitions :: MonadNakadi m
                    => Config -> EventTypeName -> m [Partition]
eventTypePartitions config eventTypeName =
  httpJsonBody config ok200 []
  (setRequestPath (path eventTypeName Nothing))

-- | GET to /event-types/NAME/partitions/PARTITION
eventTypePartition :: MonadNakadi m
                   => Config -> EventTypeName -> PartitionName -> m Partition
eventTypePartition config eventTypeName partitionName =
  httpJsonBody config ok200 []
  (setRequestPath (path eventTypeName (Just partitionName)))
