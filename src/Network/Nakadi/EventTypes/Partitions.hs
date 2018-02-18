{-|
Module      : Network.Nakadi.EventTypes.Partitions
Description : Implementation of Nakadi Partitions API
Copyright   : (c) Moritz Clasmeier 2017, 2018
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
  , eventTypePartition
  ) where

import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Prelude

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
eventTypePartitions
  :: MonadNakadi b m
  => EventTypeName -- ^ Name of Event Type
  -> m [Partition] -- ^ Partition Information
eventTypePartitions eventTypeName = do
  config <- nakadiAsk
  httpJsonBody ok200 [(status404, errorEventTypeNotFound)] $
    (includeFlowId config
     . setRequestPath (path eventTypeName Nothing))

-- | @GET@ to @\/event-types\/EVENT-TYPE\/partitions\/PARTITION@.
-- Retrieves information about a single partition.
eventTypePartition
  :: MonadNakadi b m
  => EventTypeName -- ^ Name of Event Type
  -> PartitionName -- ^ Name of Partition to look up
  -> m Partition   -- ^ Partition Information
eventTypePartition eventTypeName partitionName = do
  config <- nakadiAsk
  httpJsonBody ok200 [] $
    (includeFlowId config
     . setRequestPath (path eventTypeName (Just partitionName)))
