{-# OPTIONS_HADDOCK prune           #-} -- FIXME, workaround for Haddock bug.

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
import           Network.Nakadi.Internal.TH

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Service
import           Network.Nakadi.Internal.Types.Subscription

class HasNakadiConfig s a where
  nakadiConfig :: Lens' s a

makeNakadiLenses ''Config
makeNakadiLenses ''Cursor
makeNakadiLenses ''EventStreamBatch
makeNakadiLenses ''SubscriptionEventStreamBatch
makeNakadiLenses ''Event
makeNakadiLenses ''Metadata
makeNakadiLenses ''Partition
makeNakadiLenses ''CursorDistanceQuery
makeNakadiLenses ''CursorDistanceResult
makeNakadiLenses ''Timestamp
makeNakadiLenses ''SubscriptionEventStream
makeNakadiLenses ''SubscriptionEventStreamContext
makeNakadiLenses ''EventTypeSchema
makeNakadiLenses ''EventType
makeNakadiLenses ''EventTypeSchemasResponse
makeNakadiLenses ''PaginationLink
makeNakadiLenses ''PaginationLinks
makeNakadiLenses ''SubscriptionEventTypeStatsResult
makeNakadiLenses ''ConsumeParameters
makeNakadiLenses ''SubscriptionCursorCommit
makeNakadiLenses ''CursorCommit
makeNakadiLenses ''SubscriptionsListResponse
makeNakadiLenses ''Subscription

instance HasNakadiId StreamId Text where
  id f (StreamId a) = StreamId <$> f a

instance HasNakadiId SubscriptionId UUID where
  id f (SubscriptionId a) = SubscriptionId <$> f a

instance HasNakadiId EventId UUID where
  id f (EventId a) = EventId <$> f a

class HasNakadiUTCTime s a where
  utcTime :: Lens' s a

instance HasNakadiUTCTime Timestamp UTCTime where
  utcTime f (Timestamp t) = Timestamp <$> f t

class HasNakadiSubscriptionCursor s where
  subscriptionCursor :: Getter s SubscriptionCursor

instance HasNakadiSubscriptionCursor SubscriptionCursor where
  subscriptionCursor = identity

instance HasNakadiSubscriptionCursor (SubscriptionEventStreamBatch a) where
  subscriptionCursor = cursor
