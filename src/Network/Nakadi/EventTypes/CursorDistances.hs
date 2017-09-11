{-|
Module      : Network.Nakadi.EventTypes.CursorDistances
Description : Implementation of Nakadi CursorDistances API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the
@\/event-types\/EVENT-TYPE\/cursor-distances@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes.CursorDistances
  ( eventTypeCursorsDistance'
  , eventTypeCursorsDistanceR'
  , eventTypeCursorDistance
  , eventTypeCursorDistanceR
  )  where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/cursor-distances"

-- | Query for distance between cursors. Low level call.
eventTypeCursorsDistance' ::
  MonadNakadi m
  => Config                   -- ^ Configuration
  -> EventTypeName            -- ^ Event Type
  -> [CursorDistanceQuery]    -- ^ List of cursor-distance-queries
  -> m [CursorDistanceResult] -- ^ List of cursor-distance-results
eventTypeCursorsDistance' config eventTypeName cursorDistanceQuery =
  httpJsonBody config ok200 []
  (setRequestMethod "POST"
   . setRequestPath (path eventTypeName)
   . setRequestBodyJSON cursorDistanceQuery)

-- | Query for distance between cursors. Low level call, retrieving
-- configuration from environment.
eventTypeCursorsDistanceR' ::
  MonadNakadiEnv r m
  => EventTypeName            -- ^ Event Type
  -> [CursorDistanceQuery]    -- ^ List of cursor-distance-queries
  -> m [CursorDistanceResult] -- ^ List of cursor-distance-results
eventTypeCursorsDistanceR' eventTypeName cursorDistanceQuery = do
  config <- asks (view L.nakadiConfig)
  eventTypeCursorsDistance' config eventTypeName cursorDistanceQuery

-- | Given two cursors, compute the distance between these cursors.
eventTypeCursorDistance ::
  MonadNakadi m
  => Config        -- ^ Configuration
  -> EventTypeName -- ^ Event Type
  -> Cursor        -- ^ First cursor
  -> Cursor        -- ^ Second cursor
  -> m Int64       -- ^ Resulting difference between first and second
                   -- cursor
eventTypeCursorDistance config eventTypeName cursor cursor' =
  let cursorDistanceQuery = CursorDistanceQuery { _initialCursor = cursor
                                                , _finalCursor   = cursor' }
  in eventTypeCursorsDistance' config eventTypeName [cursorDistanceQuery] >>= \case
    distanceResult:_ -> return $ distanceResult^.L.distance
    _                -> throwIO CursorDistanceNoResult

-- | Given two cursors, compute the distance between these cursors,
-- retrieving configuration from environment.
eventTypeCursorDistanceR ::
  MonadNakadiEnv r m
  => EventTypeName -- ^ Event Type
  -> Cursor        -- ^ First cursor
  -> Cursor        -- ^ Second cursor
  -> m Int64       -- ^ Resulting difference between first and second
                   -- cursor
eventTypeCursorDistanceR eventTypeName cursor cursor' = do
  config <- asks (view L.nakadiConfig)
  eventTypeCursorDistance config eventTypeName cursor cursor'
