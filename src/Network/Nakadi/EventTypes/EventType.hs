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
  , eventTypeUpdate
  , eventTypeDelete
  ) where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName = "/event-types/" <> encodeUtf8 (unEventTypeName eventTypeName)

-- | Retrieves an 'EventType' by its 'EventTypeName'. @GET@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeGet ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> m EventType   -- ^ Event Type information
eventTypeGet eventTypeName =
  httpJsonBody ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "GET"
   . setRequestPath (path eventTypeName))

-- | Updates an event type given its 'EventTypeName' and its new
-- 'EventType' description. @PUT@ to @\/event-types\/EVENT-TYPE@.
eventTypeUpdate ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> EventType     -- ^ Event Type Settings
  -> m ()
eventTypeUpdate eventTypeName eventType =
  httpJsonNoBody ok200 []
  (setRequestMethod "PUT"
   . setRequestPath (path eventTypeName)
   . setRequestBodyJSON eventType)

-- | Deletes an event type given its 'EventTypeName'. @DELETE@ to
-- @\/event-types\/EVENT-TYPE@.
eventTypeDelete ::
  MonadNakadiEnv b m
  => EventTypeName -- ^ Name of Event Type
  -> m ()
eventTypeDelete eventTypeName =
  httpJsonNoBody ok200 [(status404, errorEventTypeNotFound)]
  (setRequestMethod "DELETE" . setRequestPath (path eventTypeName))
