{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Network.Nakadi.Internal.Lenses where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Data.Text                                  (Text)
import           Data.Time.Clock
import           Data.UUID                                  (UUID)
import qualified Network.HTTP.Client                        as HTTP

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription


makeFieldsNoPrefix ''Config

makeFieldsNoPrefix ''Cursor
makeFieldsNoPrefix ''EventStreamBatch
makeFieldsNoPrefix ''SubscriptionEventStreamBatch
makeFieldsNoPrefix ''Event
makeFieldsNoPrefix ''Metadata
makeFieldsNoPrefix ''Partition
makeFieldsNoPrefix ''CursorDistanceQuery
makeFieldsNoPrefix ''CursorDistanceResult
makeFieldsNoPrefix ''Timestamp
makeFieldsNoPrefix ''SubscriptionEventStream
makeFieldsNoPrefix ''SubscriptionEventStreamContext
makeFieldsNoPrefix ''EventTypeSchema
makeFieldsNoPrefix ''EventTypeSchemasResponse
makeFieldsNoPrefix ''PaginationLink
makeFieldsNoPrefix ''PaginationLinks
makeFieldsNoPrefix ''SubscriptionEventTypeStatsResult
makeFieldsNoPrefix ''ConsumeParameters
makeFieldsNoPrefix ''SubscriptionCursorCommit
makeFieldsNoPrefix ''CursorCommit

class HasNakadiConfig s a where
  nakadiConfig :: Lens' s a

class HasId s a | s -> a where
  id :: Lens' s a

instance HasId StreamId Text where
  id f (StreamId a) = StreamId <$> f a

instance HasId SubscriptionId UUID where
  id f (SubscriptionId a) = SubscriptionId <$> f a

instance HasId EventId UUID where
  id f (EventId a) = EventId <$> f a

class HasCheckResponse s a | s -> a where
  checkResponse :: Lens' s a

instance HasCheckResponse Request (Request -> Response HTTP.BodyReader -> IO ()) where
  checkResponse f request =
    (\a -> request { HTTP.checkResponse = a }) <$> f (HTTP.checkResponse request)

class HasSubscriptionCursor s where
  subscriptionCursor :: Getter s SubscriptionCursor

instance HasSubscriptionCursor SubscriptionCursor where
  subscriptionCursor = identity

instance HasSubscriptionCursor (SubscriptionEventStreamBatch a) where
  subscriptionCursor = cursor

class HasUTCTime s a where
  utcTime :: Lens' s a

instance HasUTCTime Timestamp UTCTime where
  utcTime f (Timestamp t) = Timestamp <$> f t
