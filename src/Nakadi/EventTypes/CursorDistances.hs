{-# LANGUAGE LambdaCase #-}

module Nakadi.EventTypes.CursorDistances
  ( eventTypeCursorsDistance'
  , eventTypeCursorDistance
  )  where

import           Nakadi.Internal.Prelude

import           Control.Lens
import           Nakadi.Internal.Http
import qualified Nakadi.Internal.Lenses  as L

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName) <> "/cursor-distances"

-- | Query for distance between cursors. Low level call.
eventTypeCursorsDistance' :: MonadNakadi m
                          => Config
                          -> EventTypeName
                          -> [CursorDistanceQuery]
                          -> m [CursorDistanceResult]
eventTypeCursorsDistance' config eventTypeName cursorDistanceQuery =
  httpJsonBody config ok200 []
  (setRequestMethod "POST"
   . setRequestPath (path eventTypeName)
   . setRequestBodyJSON cursorDistanceQuery)

-- | Given two cursors, compute the distance between these cursors.
eventTypeCursorDistance :: MonadNakadi m
                        => Config
                        -> EventTypeName
                        -> Cursor
                        -> Cursor
                        -> m Int64
eventTypeCursorDistance config eventTypeName cursor cursor' =
  let cursorDistanceQuery = CursorDistanceQuery { _initialCursor = cursor
                                                , _finalCursor   = cursor' }
  in eventTypeCursorsDistance' config eventTypeName [cursorDistanceQuery] >>= \case
    distanceResult:_ -> return $ distanceResult^.L.distance
    _                -> throwIO CursorDistanceNoResult
