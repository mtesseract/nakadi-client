{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Nakadi.EventTypes.Events
  ( eventTypeSource
  ) where

import           Nakadi.Internal.Prelude

import           Conduit
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy    as ByteString.Lazy
import           Nakadi.Internal.Config
import           Nakadi.Internal.Http
import qualified Nakadi.Internal.Lenses  as L

-- | GET to /event-types/NAME/evets. Returns Conduit for event batch consumption.
eventTypeSource :: (MonadNakadi m, MonadResource m, MonadBaseControl IO m, MonadMask m, FromJSON a)
                => Config
                -> Maybe ConsumeParameters
                -> EventTypeName
                -> Maybe [Cursor]
                -> m (ConduitM () (EventStreamBatch a) m ())
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
