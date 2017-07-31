-- | Schemas

module Nakadi.EventTypes.Schemas
  ( eventTypeSchemas'
  , eventTypeSchema
  ) where

import           Nakadi.Internal.Prelude

import           Nakadi.Internal.Http

path :: EventTypeName -> Maybe SchemaVersion -> ByteString
path eventTypeName maybeSchemaVersion =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/schemas"
  <> (case maybeSchemaVersion of
        Just schemaVersion -> "/" <> encodeUtf8 (unSchemaVersion schemaVersion)
        Nothing            -> "")

eventTypeSchemas' :: MonadNakadi m
                  => Config
                  -> EventTypeName
                  -> Maybe Offset
                  -> Maybe Limit
                  -> m EventTypeSchemasResponse
eventTypeSchemas' config eventTypeName offset limit =
  httpJsonBody config ok200 []
  (setRequestMethod "GET"
   . setRequestPath (path eventTypeName Nothing)
   . setRequestQueryParameters [ ("offset", offset')
                               , ("limit",  limit') ])
  where offset' = encodeUtf8 (tshow (maybe defaultOffset unOffset offset))
        limit'  = encodeUtf8 (tshow (maybe defaultLimit  unLimit  limit))
        defaultOffset =  0
        defaultLimit  = 20

eventTypeSchema :: MonadNakadi m
                => Config
                -> EventTypeName
                -> SchemaVersion
                -> m EventTypeSchema
eventTypeSchema config eventTypeName schemaVersion =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath (path eventTypeName (Just schemaVersion)))
