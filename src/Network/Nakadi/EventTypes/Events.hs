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
{-# LANGUAGE TypeApplications       #-}

module Network.Nakadi.EventTypes.Events
  ( eventsPublish
  ) where

import           Network.Nakadi.Internal.Prelude

import qualified Control.Lens as L
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.HashMap.Strict   (toList)
import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Types.Service
import           Data.Proxy
import           Data.UUID.V4          (nextRandom)

path :: EventTypeName -> ByteString
path eventTypeName =
  "/event-types/"
  <> encodeUtf8 (unEventTypeName eventTypeName)
  <> "/events"

class NamedEventType a where
  eventTypeName :: Proxy a -> EventTypeName

class (ToJSON a, NamedEventType a)=> (HasMetadata a) where
  metadata :: Monad m => Proxy a -> m EventMetadata

-- Type class to enforce that an event must be published as a DataChangeEvent
class (ToJSON a, NamedEventType a, HasMetadata a) => (ToDataChangeEvent a) where
  dataOp :: a -> DataOp
  dataType :: a -> Text

-- Type class to enforce that an event must be published as a BusinessEvent
class (ToJSON a, NamedEventType a, HasMetadata a) => ToBusinessEvent a

-- | Publish a list of data change events and
publishDataChangeEvents ::
    forall a b m.(MonadNakadi b m, ToDataChangeEvent a)
  => [a]
  -> m ()
publishDataChangeEvents objects =
  do
    config <- nakadiAsk
    let etn = eventTypeName (Proxy :: Proxy a)
    events <- traverse createDataChangeEvent objects
    runNakadiT config (eventsPublish etn events)

-- | Publish a list of business events and
publishBusinessEvents ::
    forall a b m. (MonadNakadi b m, ToBusinessEvent a)
  => [a]
  -> m ()
publishBusinessEvents objects =
  do
    config <- nakadiAsk
    let etn = eventTypeName (Proxy :: Proxy a)
    events <- traverse createBusinessEvent objects
    runNakadiT config (eventsPublish etn events)
    
-- | Add metadata to make a business event
createBusinessEvent :: forall a m. (Monad m, ToBusinessEvent a) => a -> m Value
createBusinessEvent a = do
  eventMetadata <- metadata @a (Proxy :: Proxy a)
  pure $ object (
    ["metadata" .= toJSON eventMetadata ] ++
    fields (toJSON a))

-- | Add metadata and datachange fields to make a datachange event
createDataChangeEvent :: forall a m. (Monad m, ToDataChangeEvent a) => a -> m Value
createDataChangeEvent a = do
  eventMetadata <- metadata @a (Proxy :: Proxy a)
  pure $ object
      [ "metadata"  .= toJSON eventMetadata
      , "data_op"   .= toJSON (dataOp a)
      , "data_type" .= toJSON (dataType a)
      , "data"      .= toJSON a
      ]

fields :: Value -> [(Text, Value)]
fields a =
    case a L.^? _Object of
        Just fs -> toList fs
        Nothing -> []

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
