{-|
Module      : Network.Nakadi.EventTypes.Partitions
Description : Implementation of Nakadi Partitions API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/event-types\/EVENT-TYPE\/partitions@
API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes.Partitions
  ( eventTypePartitions
  , eventTypePartitionsR
  , eventTypePartition
  , eventTypePartitionR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L

path :: EventTypeName -> Maybe PartitionName -> ByteString
path eventTypeName maybePartitionName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/partitions"
  <> (case maybePartitionName of
        Just partitionName -> "/" <> encodeUtf8 (unPartitionName partitionName)
        Nothing            -> "")

-- | @GET@ to @\/event-types\/EVENT-TYPE\/partitions@. Retrieves
-- information about all partitions.
eventTypePartitions ::
  MonadNakadi m
  => Config        -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> m [Partition] -- ^ Partition Information
eventTypePartitions config eventTypeName =
  httpJsonBody config ok200 []
  (setRequestPath (path eventTypeName Nothing))

-- | @GET@ to @\/event-types\/EVENT-TYPE\/partitions@. Retrieves
-- information about all partitions, using the configuration contained
-- in the environment.
eventTypePartitionsR ::
  MonadNakadiEnv r m
  => EventTypeName -- ^ Name of Event Type
  -> m [Partition] -- ^ Partition Information
eventTypePartitionsR eventTypeName = do
  config <- asks (view L.nakadiConfig)
  eventTypePartitions config eventTypeName

-- | @GET@ to @\/event-types\/EVENT-TYPE\/partitions\/PARTITION@.
-- Retrieves information about a single partition.
eventTypePartition ::
  MonadNakadi m
  => Config        -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> PartitionName -- ^ Name of Partition to look up
  -> m Partition   -- ^ Partition Information
eventTypePartition config eventTypeName partitionName =
  httpJsonBody config ok200 []
  (setRequestPath (path eventTypeName (Just partitionName)))

-- | @GET@ to @\/event-types\/EVENT-TYPE\/partitions\/PARTITION@.
-- Retrieves information about a single partition, using the
-- configuration contained in the environment.
eventTypePartitionR ::
  MonadNakadiEnv r m
  => EventTypeName -- ^ Name of Event Type
  -> PartitionName -- ^ Name of Partition to look up
  -> m Partition   -- ^ Partition Information
eventTypePartitionR eventTypeName partitionName = do
  config <- asks (view L.nakadiConfig)
  eventTypePartition config eventTypeName partitionName
