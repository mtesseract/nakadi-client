{-# LANGUAGE RecordWildCards #-}

module Network.Nakadi.EventTypes.ShiftedCursors
  ( eventTypeCursorsShift'
  , eventTypeCursorsShift
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName) <> "/shifted-cursors"

-- | POST to /event-types/NAME/shifted-cursors.
eventTypeCursorsShift' :: MonadNakadi m
                       => Config
                       -> EventTypeName
                       -> [ShiftedCursor]
                       -> m [Cursor]
eventTypeCursorsShift' config eventTypeName cursors =
  httpJsonBody config ok200 []
  (setRequestMethod "POST" . setRequestPath (path eventTypeName) . setRequestBodyJSON cursors)

eventTypeCursorsShift :: MonadNakadi m
                      => Config
                      -> EventTypeName
                      -> [Cursor]
                      -> Int64
                      -> m [Cursor]
eventTypeCursorsShift config eventTypeName cursors n =
  eventTypeCursorsShift' config eventTypeName (map makeShiftCursor cursors)

  where makeShiftCursor Cursor { .. } = ShiftedCursor { _partition = _partition
                                                      , _offset    = _offset
                                                      , _shift     = n }
