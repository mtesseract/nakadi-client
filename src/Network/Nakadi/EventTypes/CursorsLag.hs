{-|
Module      : Network.Nakadi.EventTypes.CursorsLag
Description : Implementation of Nakadi CursorsLag API
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the
@\/event-types\/EVENT-TYPE\/cursors-lag@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes.CursorsLag
  ( cursorsLag'
  , cursorsLag
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict                 as Map
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Internal.Util

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/cursors-lag"

-- | @POST@ to @\/event-types\/EVENT-TYPE\/cursors-lag@. Low level
-- interface.
cursorsLag' ::
  MonadNakadi b m
  => EventTypeName -- ^ Event Type
  -> [Cursor]      -- ^ Cursors for which to compute the lag for
  -> m [Partition] -- ^ Resulting partition information containing
                   -- information about unconsumed events.
cursorsLag' eventTypeName cursors = do
  config <- nakadiAsk
  httpJsonBody ok200 [] $
    (setRequestMethod "POST"
     . includeFlowId config
     . setRequestPath (path eventTypeName)
     . setRequestBodyJSON cursors)

-- | @POST@ to @\/event-types\/EVENT-TYPE\/cursors-lag@. High level
-- interface.
cursorsLag ::
  MonadNakadi b m
  => EventTypeName                  -- ^ Event Type
  -> Map PartitionName CursorOffset -- ^ Cursor offsets associated to
                                    -- partitions.
  -> m (Map PartitionName Int64)    -- ^ Cursor lags associated to partitions.
cursorsLag eventTypeName cursorsMap = do
  partitionStats <- cursorsLag' eventTypeName cursors
  return $ partitionStats & map ((view L.partition &&& view L.unconsumedEvents) >>> sequenceSnd)
                          & catMaybes
                          & Map.fromList
  where cursors = map (uncurry Cursor) (Map.toList cursorsMap)
