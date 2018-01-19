{-|
Module      : Network.Nakadi.EventTypes.Events
Description : Implementation of Nakadi Events API
Copyright   : (c) Moritz Schulte 2017, 2018
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
{-# LANGUAGE TypeFamilies          #-}

module Network.Nakadi.EventTypes.Events
  ( eventsProcessConduit
  , eventsProcess
  , eventsPublish
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import           Data.Void
import           Network.HTTP.Client             (responseBody)
import           Network.Nakadi.Internal.Config
import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/events"

eventsProcess
  :: ( MonadNakadi b m
     , FromJSON a
     , MonadNakadiHttpStream b m
     , NakadiHttpStreamConstraint m
     , MonadMask m
     )
  => Maybe ConsumeParameters
  -> EventTypeName
  -> Maybe [Cursor]
  -> (EventStreamBatch a -> m r)
  -> m r
eventsProcess maybeConsumeParameters eventTypeName maybeCursors processor =
  eventsProcess maybeConsumeParameters eventTypeName maybeCursors processor

eventsProcessConduit
  :: ( MonadNakadi b m
     , FromJSON a
     , MonadNakadiHttpStream b m
     , NakadiHttpStreamConstraint m
     , MonadMask m
     )
  => Maybe ConsumeParameters
  -> EventTypeName
  -> Maybe [Cursor]
  -> ConduitM (EventStreamBatch a) Void m r
  -> m r
eventsProcessConduit maybeConsumeParameters eventTypeName maybeCursors consumer = do
  config <- nakadiAsk
  let consumeParams = fromMaybe defaultConsumeParameters maybeConsumeParameters
      queryParams   = buildSubscriptionConsumeQueryParameters consumeParams
  httpJsonBodyStream ok200 [ (status429, errorTooManyRequests)
                           , (status429, errorEventTypeNotFound) ]
    (setRequestPath (path eventTypeName)
     . setRequestQueryParameters queryParams
     . addCursors) $
    handler config

  where addCursors = case maybeCursors of
          Just cursors -> let cursors' = ByteString.Lazy.toStrict (encode cursors)
                          in addRequestHeader "X-Nakadi-Cursors" cursors'
          Nothing      -> identity

        handler config response =
          responseBody response
          .| linesUnboundedAsciiC
          .| conduitDecode config
          $$ consumer

-- | @POST@ to @\/event-types\/NAME\/events@. Publishes a batch of
-- events for the specified event type.
eventsPublish ::
  (MonadNakadi b m, ToJSON a)
  => EventTypeName
  -> Maybe FlowId
  -> [a]
  -> m ()
eventsPublish eventTypeName maybeFlowId eventBatch =
  httpJsonNoBody status200
  [ (Status 207 "Multi-Status", errorBatchPartiallySubmitted)
  , (status422, errorBatchNotSubmitted) ]
  (setRequestMethod "POST"
   . setRequestPath (path eventTypeName)
   . maybe identity (addRequestHeader "X-Flow-Id" . encodeUtf8 . unFlowId) maybeFlowId
   . setRequestBodyJSON eventBatch)
