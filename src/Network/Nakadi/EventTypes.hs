{-|
Module      : Network.Nakadi.EventTypes
Description : Implementation of Nakadi EventTypes API
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/event-types@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes
  ( module Network.Nakadi.EventTypes.CursorDistances
  , module Network.Nakadi.EventTypes.CursorsLag
  , module Network.Nakadi.EventTypes.Events
  , module Network.Nakadi.EventTypes.EventType
  , module Network.Nakadi.EventTypes.Partitions
  , module Network.Nakadi.EventTypes.ShiftedCursors
  , module Network.Nakadi.EventTypes.Schemas
  , eventTypesList
  , eventTypeCreate
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.EventTypes.CursorDistances
import           Network.Nakadi.EventTypes.CursorsLag
import           Network.Nakadi.EventTypes.Events
import           Network.Nakadi.EventTypes.EventType
import           Network.Nakadi.EventTypes.Partitions
import           Network.Nakadi.EventTypes.Schemas
import           Network.Nakadi.EventTypes.ShiftedCursors
import           Network.Nakadi.Internal.Http

path :: ByteString
path = "/event-types"

-- | @GET@ to @\/event-types@. Retrieves a list of all registered
-- event types.
eventTypesList ::
  MonadNakadi b m
  => m [EventType] -- ^ Registered Event Types
eventTypesList =
  httpJsonBody status200 []
  (setRequestMethod "GET" . setRequestPath path)

-- | @POST@ to @\/event-types@. Creates a new event type.
eventTypeCreate ::
  MonadNakadi b m
  => EventType -- ^ Event Type to create
  -> m ()
eventTypeCreate eventType =
  httpJsonNoBody status201 []
  (setRequestMethod "POST"
   . setRequestPath path
   . setRequestBodyJSON eventType)
