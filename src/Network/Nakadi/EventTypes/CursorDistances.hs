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
  ( cursorsDistance'
  , cursorsDistanceR'
  , cursorDistance
  , cursorDistanceR
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
cursorsDistance' ::
  MonadNakadi b m
  => Config' b                -- ^ Configuration
  -> EventTypeName            -- ^ Event Type
  -> [CursorDistanceQuery]    -- ^ List of cursor-distance-queries
  -> m [CursorDistanceResult] -- ^ List of cursor-distance-results
cursorsDistance' config eventTypeName cursorDistanceQuery =
  httpJsonBody config ok200 []
  (setRequestMethod "POST"
   . setRequestPath (path eventTypeName)
   . setRequestBodyJSON cursorDistanceQuery)

-- | Query for distance between cursors. Low level call, retrieving
-- configuration from environment.
cursorsDistanceR' ::
  MonadNakadiEnv b m
  => EventTypeName            -- ^ Event Type
  -> [CursorDistanceQuery]    -- ^ List of cursor-distance-queries
  -> m [CursorDistanceResult] -- ^ List of cursor-distance-results
cursorsDistanceR' eventTypeName cursorDistanceQuery = do
  config <- nakadiAsk
  cursorsDistance' config eventTypeName cursorDistanceQuery

-- | Given two cursors, compute the distance between these cursors.
cursorDistance ::
  MonadNakadi b m
  => Config' b     -- ^ Configuration
  -> EventTypeName -- ^ Event Type
  -> Cursor        -- ^ First cursor
  -> Cursor        -- ^ Second cursor
  -> m Int64       -- ^ Resulting difference between first and second
                   -- cursor
cursorDistance config eventTypeName cursor cursor' =
  let cursorDistanceQuery = CursorDistanceQuery { _initialCursor = cursor
                                                , _finalCursor   = cursor' }
  in cursorsDistance' config eventTypeName [cursorDistanceQuery] >>= \case
    distanceResult:_ -> return $ distanceResult^.L.distance
    _                -> throwIO CursorDistanceNoResult

-- | Given two cursors, compute the distance between these cursors,
-- retrieving configuration from environment.
cursorDistanceR ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Event Type
  -> Cursor        -- ^ First cursor
  -> Cursor        -- ^ Second cursor
  -> m Int64       -- ^ Resulting difference between first and second
                   -- cursor
cursorDistanceR eventTypeName cursor cursor' = do
  config <- nakadiAsk
  cursorDistance config eventTypeName cursor cursor'
