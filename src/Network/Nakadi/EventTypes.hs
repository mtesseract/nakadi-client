{-|
Module      : Network.Nakadi.EventTypes
Description : Implementation of Nakadi EventTypes API
Copyright   : (c) Moritz Schulte 2017
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
  , eventTypesListR
  , eventTypeCreate
  , eventTypeCreateR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Network.Nakadi.EventTypes.CursorDistances
import           Network.Nakadi.EventTypes.CursorsLag
import           Network.Nakadi.EventTypes.Events
import           Network.Nakadi.EventTypes.EventType
import           Network.Nakadi.EventTypes.Partitions
import           Network.Nakadi.EventTypes.Schemas
import           Network.Nakadi.EventTypes.ShiftedCursors
import           Network.Nakadi.Internal.Http

import qualified Network.Nakadi.Internal.Lenses            as L

path :: ByteString
path = "/event-types"

-- | @GET@ to @\/event-types@. Retrieves a list of all registered
-- event types.
eventTypesList ::
  MonadNakadi m
  => Config        -- ^ Configuration
  -> m [EventType] -- ^ Registered Event Types
eventTypesList config =
  httpJsonBody config status200 []
  (setRequestMethod "GET" . setRequestPath path)

-- | @GET@ to @\/event-types@. Retrieves a list of all registered
-- event types, using the configuration contained in the environment.
eventTypesListR ::
  MonadNakadiEnv r m
  => m [EventType] -- ^ Registered Event Types
eventTypesListR = do
  config <- asks (view L.nakadiConfig)
  eventTypesList config

-- | @POST@ to @\/event-types@. Creates a new event type.
eventTypeCreate ::
  MonadNakadi m
  => Config    -- ^ Configuration
  -> EventType -- ^ Event Type to create
  -> m ()
eventTypeCreate config eventType =
  httpJsonNoBody config status201 []
  (setRequestMethod "POST" . setRequestPath path . setRequestBodyJSON eventType)

-- | @POST@ to @\/event-types@. Creates a new event type. Uses the
-- configuration from the environment.
eventTypeCreateR ::
  MonadNakadiEnv r m
  => EventType -- ^ Event Type to create
  -> m ()
eventTypeCreateR eventType = do
  config <- asks (view L.nakadiConfig)
  eventTypeCreate config eventType
