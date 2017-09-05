module Network.Nakadi.EventTypes.EventType
  ( eventTypeGet
  , eventTypeUpdate
  , eventTypeDelete
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName)

-- | GET /event-types/NAME.
eventTypeGet :: MonadNakadi m => Config -> EventTypeName -> m EventType
eventTypeGet config eventTypeName =
  httpJsonBody config ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "GET" . setRequestPath (path eventTypeName))

-- | PUT to /event-types/NAME.
eventTypeUpdate :: MonadNakadi m => Config -> EventTypeName -> EventType -> m ()
eventTypeUpdate config eventTypeName eventType =
  httpJsonNoBody config ok200 []
  (setRequestMethod "PUT" . setRequestPath (path eventTypeName) . setRequestBodyJSON eventType)

-- | DELETE to /event-types/NAME.
eventTypeDelete :: MonadNakadi m => Config -> EventTypeName -> m ()
eventTypeDelete config eventTypeName =
  httpJsonNoBody config ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path eventTypeName))
