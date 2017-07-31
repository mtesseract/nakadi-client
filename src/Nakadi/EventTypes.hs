module Nakadi.EventTypes
  ( module Nakadi.EventTypes.CursorDistances
  , module Nakadi.EventTypes.CursorsLag
  , module Nakadi.EventTypes.Events
  , module Nakadi.EventTypes.Partitions
  , module Nakadi.EventTypes.ShiftedCursors
  , module Nakadi.EventTypes.Schemas
  , eventTypesGet
  , eventTypeCreate
  ) where

import           Nakadi.Internal.Prelude

import           Nakadi.EventTypes.CursorDistances
import           Nakadi.EventTypes.CursorsLag
import           Nakadi.EventTypes.Events
import           Nakadi.EventTypes.Partitions
import           Nakadi.EventTypes.Schemas
import           Nakadi.EventTypes.ShiftedCursors
import           Nakadi.Internal.Http

path :: ByteString
path = "/event-types"

-- | GET to /event-types
eventTypesGet :: MonadNakadi m => Config -> m [EventType]
eventTypesGet config =
  httpJsonBody config status200 []
  (setRequestMethod "GET" . setRequestPath path)

-- | POST to /event-types.
eventTypeCreate :: MonadNakadi m => Config -> EventType -> m ()
eventTypeCreate config eventType =
  httpJsonNoBody config status201 []
  (setRequestMethod "POST" . setRequestPath path . setRequestBodyJSON eventType)
