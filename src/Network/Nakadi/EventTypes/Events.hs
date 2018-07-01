{-|
Module      : Network.Nakadi.EventTypes.Events
Description : Implementation of Nakadi Events API
Copyright   : (c) Moritz Clasmeier 2017, 2018
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
  ( eventsPublish
  ) where

import           Network.Nakadi.Internal.Prelude

import           Data.Aeson
import           Network.Nakadi.Internal.Http

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/events"

-- | @POST@ to @\/event-types\/NAME\/events@. Publishes a batch of
-- events for the specified event type.
eventsPublish
  :: (MonadNakadi b m, ToJSON a)
  => EventTypeName
  -> [a]
  -> m ()
eventsPublish eventTypeName eventBatch = do
  config <- nakadiAsk
  httpJsonNoBody status200
    [ (Status 207 "Multi-Status", errorBatchPartiallySubmitted)
    , (status422, errorBatchNotSubmitted) ] $
    (setRequestMethod "POST"
     . includeFlowId config
     . setRequestPath (path eventTypeName)
     . setRequestBodyJSON eventBatch)
