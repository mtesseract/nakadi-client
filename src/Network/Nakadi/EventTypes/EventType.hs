{-|
Module      : Network.Nakadi.EventTypes.EventType
Description : Implementation of Nakadi EventTypes API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/event-types\/EVENT-TYPE@ API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.EventTypes.EventType
  ( eventTypeGet
  , eventTypeGetR
  , eventTypeUpdate
  , eventTypeUpdateR
  , eventTypeDelete
  , eventTypeDeleteR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName)

-- | Retrieves an 'EventType' by its 'EventTypeName'. @GET@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeGet ::
  MonadNakadi b m
  => Config' b     -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> m EventType   -- ^ Event Type information
eventTypeGet config eventTypeName =
  runNakadiT config $ eventTypeGetR eventTypeName

-- | Retrieves an 'EventType' by its 'EventTypeName', using the
-- configuration found in the environment. @GET@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeGetR ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> m EventType   -- ^ Event Type information
eventTypeGetR eventTypeName =
  httpJsonBody ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "GET"
   . setRequestPath (path eventTypeName))

-- | Updates an event type given its 'EventTypeName' and its new
-- 'EventType' description. @PUT@ to @\/event-types\/EVENT-TYPE@.
eventTypeUpdate ::
  MonadNakadi b m
  => Config' b     -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> EventType     -- ^ Event Type Settings
  -> m ()
eventTypeUpdate config eventTypeName eventType =
  runNakadiT config $ eventTypeUpdateR eventTypeName eventType

-- | Updates an event type given its 'EventTypeName' and its new
-- 'EventType' description, using the configuration found in the
-- environment. @PUT@ to @\/event-types\/EVENT-TYPE@.
eventTypeUpdateR ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> EventType     -- ^ Event Type Settings
  -> m ()
eventTypeUpdateR eventTypeName eventType =
  httpJsonNoBody ok200 []
  (setRequestMethod "PUT"
   . setRequestPath (path eventTypeName)
   . setRequestBodyJSON eventType)

-- | Deletes an event type given its 'EventTypeName'. @DELETE@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeDelete ::
  MonadNakadi b m
  => Config' b     -- ^ Configuration
  -> EventTypeName -- ^ Name of Event Type
  -> m ()
eventTypeDelete config eventTypeName =
  runNakadiT config $ eventTypeDeleteR eventTypeName

-- | Deletes an event type given its 'EventTypeName', using the
-- configuration found in the environment. @DELETE@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeDeleteR ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> m ()
eventTypeDeleteR eventTypeName =
  httpJsonNoBody ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path eventTypeName))
