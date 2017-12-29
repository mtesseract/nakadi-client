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
  ( eventTypeSchemasGet
  , eventTypeSchemasGetR
  , eventTypeSchema
  , eventTypeSchemaR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> Maybe SchemaVersion -> ByteString
path eventTypeName maybeSchemaVersion =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/schemas"
  <> case maybeSchemaVersion of
       Just schemaVersion -> "/" <> encodeUtf8 (unSchemaVersion schemaVersion)
       Nothing            -> ""

-- | Retrieves schemas for the given 'EventTypeName' using low-level
-- paging interface. @GET@ to @\/event-types\/NAME\/schemas@.
eventTypeSchemasGet ::
  MonadNakadi b m
  => Config' b     -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> Maybe Offset
  -> Maybe Limit
  -> m EventTypeSchemasResponse
eventTypeSchemasGet config eventTypeName offset limit =
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
eventTypeSchemasGetR ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> Maybe Offset
  -> Maybe Limit
  -> m EventTypeSchemasResponse
eventTypeSchemasGetR eventTypeName offset limit = do
  config <- nakadiAsk
  eventTypeSchemasGet config eventTypeName offset limit

-- | Look up the schema of an event type given its 'EventTypeName' and
-- 'SchemaVersion'. @GET@ to
-- @\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@.
eventTypeSchema ::
  MonadNakadi b m
  => Config' b
  -> EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchema config eventTypeName schemaVersion =
  httpJsonBody config ok200 []
  (setRequestMethod "GET" . setRequestPath (path eventTypeName (Just schemaVersion)))

-- | Look up the schema of an event type given its 'EventTypeName' and
-- 'SchemaVersion', using the configuration found in the environment.
-- @GET@ to @\/event-types\/EVENT-TYPE\/schemas\/SCHEMA@.
eventTypeSchemaR ::
  MonadNakadiEnv b m
  => EventTypeName
  -> SchemaVersion
  -> m EventTypeSchema
eventTypeSchemaR eventTypeName schemaVersion = do
  config <- nakadiAsk
  eventTypeSchema config eventTypeName schemaVersion
