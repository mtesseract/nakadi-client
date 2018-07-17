{-|
Module      : Network.Nakadi.EventTypes.ShiftedCursors
Description : Implementation of Nakadi ShiftedCursors API
Copyright   : (c) Moritz Clasmeier 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the
@\/event-types\/EVENT-TYPE\/shifted-cursors@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Network.Nakadi.EventTypes.ShiftedCursors
  ( cursorsShift'
  , cursorsShift
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/shifted-cursors"

-- | @POST@ to @\/event-types\/EVENT-TYPE\/shifted-cursors@. Low level
-- interface.
cursorsShift' ::
  MonadNakadi b m
  => EventTypeName   -- ^ Event Type
  -> [ShiftedCursor] -- ^ Cursors with Shift Distances
  -> m [Cursor]      -- ^ Resulting Cursors
cursorsShift' eventTypeName cursors = do
  config <- nakadiAsk
  httpJsonBody ok200 []
    (setRequestMethod "POST"
     . includeFlowId config
     . setRequestPath (path eventTypeName)
     . setRequestBodyJSON cursors)

-- | @POST@ to @\/event-types\/EVENT-TYPE\/shifted-cursors@. High
-- level interface.
cursorsShift ::
  MonadNakadi b m
  => EventTypeName -- ^ Event Type
  -> [Cursor]      -- ^ Cursors to shift
  -> Int64         -- ^ Shift Distance
  -> m [Cursor]    -- ^ Resulting Cursors
cursorsShift eventTypeName cursors n =
  cursorsShift' eventTypeName (map makeShiftCursor cursors)

  where makeShiftCursor Cursor { .. } =
          ShiftedCursor { _partition = _partition
                        , _offset    = _offset
                        , _shift     = n }
