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

-- FIXME, needs documentation improvements.

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

-- | @GET@ to @\/event-types\/NAME\/schemas@.
eventTypeSchemas' ::
  MonadNakadi m
  => Config        -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
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

-- | @GET@ to @\/event-types\/NAME\/schemas@. Uses the configuration
-- contained in the environment.
eventTypeSchemasR' ::
  MonadNakadiEnv r m
  => EventTypeName -- ^ Name of Event Type
  -> Maybe Offset
  -> Maybe Limit
  -> m EventTypeSchemasResponse
eventTypeSchemasR' eventTypeName offset limit = do
  config <- asks (view L.nakadiConfig)
  eventTypeSchemas' config eventTypeName offset limit

-- | @GET@ to @\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@.
eventTypeSchema ::
  MonadNakadi m
  => Config
  -> EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchema config eventTypeName schemaVersion =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath (path eventTypeName (Just schemaVersion)))

eventTypeSchemaR ::
  MonadNakadiEnv r m
  => EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchemaR eventTypeName schemaVersion = do
  config <- asks (view L.nakadiConfig)
  eventTypeSchema config eventTypeName schemaVersion
