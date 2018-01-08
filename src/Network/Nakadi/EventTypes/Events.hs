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
  ( eventSource
  , eventPublish
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Internal.Lenses  as L

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/events"

-- | @GET@ to @\/event-types\/NAME\/events@. Returns Conduit source
-- for event batch consumption.
eventSource ::
  (MonadNakadiEnv b m, MonadResource m, FromJSON a, MonadBaseControl IO m
  , MonadSub b n, MonadIO n
  )
  => Maybe ConsumeParameters -- ^ Optional parameters for event consumption
  -> EventTypeName           -- ^ Name of the event type to consume
  -> Maybe [Cursor]          -- ^ Optional list of cursors; by default
                             -- consumption begins with the most
                             -- recent event
  -> m (ConduitM () (EventStreamBatch a)
        n ())                -- ^ Returns a Conduit source.
eventSource maybeParams eventTypeName maybeCursors = do
  config <- nakadiAsk
  let consumeParams = fromMaybe (config^.L.consumeParameters) maybeParams
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
  runReaderC () . snd <$>
    httpJsonBodyStream ok200 (const (Right ())) [ (status429, errorTooManyRequests)
                                                , (status429, errorEventTypeNotFound) ]
    (setRequestPath (path eventTypeName)
     . setRequestQueryParameters queryParams
     . addCursors)

    where addCursors = case maybeCursors of
            Just cursors -> let cursors' = ByteString.Lazy.toStrict (encode cursors)
                            in addRequestHeader "X-Nakadi-Cursors" cursors'
            Nothing      -> identity

-- | @POST@ to @\/event-types\/NAME\/events@. Publishes a batch of
-- events for the specified event type.
eventPublish ::
  (MonadNakadiEnv b m, ToJSON a)
  => EventTypeName
  -> Maybe FlowId
  -> [a]
  -> m ()
eventPublish eventTypeName maybeFlowId eventBatch =
  httpJsonNoBody status200
  [ (Status 207 "Multi-Status", errorBatchPartiallySubmitted)
  , (status422, errorBatchNotSubmitted) ]
  (setRequestMethod "POST"
   . setRequestPath (path eventTypeName)
   . maybe identity (addRequestHeader "X-Flow-Id" . encodeUtf8 . unFlowId) maybeFlowId
   . setRequestBodyJSON eventBatch)
