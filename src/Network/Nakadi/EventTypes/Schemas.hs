{-|
Module      : Network.Nakadi.EventTypes.Schemas
Description : Implementation of Nakadi Schemas API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the
@\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes.Schemas
  ( eventTypeSchemas'
  , eventTypeSchemasR'
  , eventTypeSchema
  , eventTypeSchemaR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L

path :: EventTypeName -> Maybe SchemaVersion -> ByteString
path eventTypeName maybeSchemaVersion =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/schemas"
  <> case maybeSchemaVersion of
       Just schemaVersion -> "/" <> encodeUtf8 (unSchemaVersion schemaVersion)
       Nothing            -> ""

-- | @GET@ to @\/event-types\/NAME\/schemas@. Retrieves list of
-- schemas for the specified event type, ordered from most recent to
-- oldest.
eventTypeSchemas' ::
  MonadNakadi m
  => Config                     -- ^ Configuration
  -> EventTypeName              -- ^ Name of Event Type
  -> Maybe Offset               -- ^ Optional page offset
  -> Maybe Limit                -- ^ Optional maximum number of schemas returned in one page
  -> m EventTypeSchemasResponse -- ^ Returns Event Type Schema along with pagination links
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

-- | @GET@ to @\/event-types\/NAME\/schemas@. Retrieves list of
-- schemas for the specified event type, ordered from most recent to
-- oldest. Uses the configuration contained in the environment.
eventTypeSchemasR' ::
  MonadNakadiEnv r m
  => EventTypeName -- ^ Name of Event Type
  -> Maybe Offset
  -> Maybe Limit
  -> m EventTypeSchemasResponse
eventTypeSchemasR' eventTypeName offset limit = do
  config <- asks (view L.nakadiConfig)
  eventTypeSchemas' config eventTypeName offset limit

-- | @GET@ to @\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@. Retrieves
-- the specified schema version (the special schema version
-- @SchemaVersion "latest"@ is supported).
eventTypeSchema ::
  MonadNakadi m
  => Config
  -> EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchema config eventTypeName schemaVersion =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath (path eventTypeName (Just schemaVersion)))

-- | @GET@ to @\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@. Retrieves
-- the specified schema version (the special schema version
-- @SchemaVersion "latest"@ is supported). Uses the configuration
-- stored in the environment.
eventTypeSchemaR ::
  MonadNakadiEnv r m
  => EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchemaR eventTypeName schemaVersion = do
  config <- asks (view L.nakadiConfig)
  eventTypeSchema config eventTypeName schemaVersion
