module Network.Nakadi.EventTypes
  ( module Network.Nakadi.EventTypes.CursorDistances
  , module Network.Nakadi.EventTypes.CursorsLag
  , module Network.Nakadi.EventTypes.Events
  , module Network.Nakadi.EventTypes.EventType
  , module Network.Nakadi.EventTypes.Partitions
  , module Network.Nakadi.EventTypes.ShiftedCursors
  , module Network.Nakadi.EventTypes.Schemas
  , eventTypesGet
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
