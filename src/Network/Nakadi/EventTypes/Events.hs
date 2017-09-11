{-|
Module      : Network.Nakadi.EventTypes.Events
Description : Implementation of Nakadi Events API
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the
@\/event-types\/EVENT-TYPE\/events@ API.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Network.Nakadi.EventTypes.Events
  ( eventTypeSource
  , eventTypeSourceR
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L

-- | @GET@ to @\/event-types\/NAME\/events@. Returns Conduit source
-- for event batch consumption.
eventTypeSource ::
  (MonadNakadi m, MonadResource m, MonadBaseControl IO m, MonadMask m, FromJSON a)
  => Config                  -- ^ Configuration parameter
  -> Maybe ConsumeParameters -- ^ Optional parameters for event consumption
  -> EventTypeName           -- ^ Name of the event type to consume
  -> Maybe [Cursor]          -- ^ Optional list of cursors; by default
                             -- consumption begins with the most
                             -- recent event
  -> m (ConduitM () (EventStreamBatch a)
        m ())                -- ^ Returns a Conduit source.
eventTypeSource config maybeParams eventType maybeCursors = do
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeParams
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
  runReaderC () . snd <$>
    httpJsonBodyStream config ok200 (const (Right ())) [ (status429, errorTooManyRequests)
                                                       , (status429, errorEventTypeNotFound) ]
    (setRequestPath ("/event-types/" <> encodeUtf8 (unEventTypeName eventType) <> "/events")
     . setRequestQueryParameters queryParams
     . case maybeCursors of
         Just cursors -> let cursors' = ByteString.Lazy.toStrict (encode cursors)
                         in addRequestHeader "X-Nakadi-Cursors" cursors'
         Nothing      -> identity)

-- | @GET@ to @\/event-types\/NAME\/events@. Returns Conduit source
-- for event batch consumption. Retrieves configuration from
-- environment.
eventTypeSourceR ::
  (MonadNakadiEnv r m, MonadResource m, MonadBaseControl IO m, MonadMask m, FromJSON a)
  => Maybe ConsumeParameters -- ^ Optional parameters for event consumption
  -> EventTypeName           -- ^ Name of the event type to consume
  -> Maybe [Cursor]          -- ^ Optional list of cursors; by default
                             -- consumption begins with the most
                             -- recent event
  -> m (ConduitM () (EventStreamBatch a)
        m ())                -- ^ Returns a Conduit source.
eventTypeSourceR maybeParams eventType maybeCursors = do
  config <- asks (view L.nakadiConfig)
  eventTypeSource config maybeParams eventType maybeCursors
