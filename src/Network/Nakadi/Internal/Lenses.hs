{-|
Module      : Network.Nakadi.Lenses
Description : Nakadi Client Library Lenses (Internal)
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements lenses for the data types contained in this
package.
-}

{-# OPTIONS_HADDOCK prune           #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Network.Nakadi.Internal.Lenses where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Data.Text                             (Text)
import           Data.Time.Clock
import           Data.UUID                             (UUID)
import           Network.Nakadi.Internal.TH

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Service

makeNakadiLenses ''Config
makeNakadiLenses ''HttpBackend
makeNakadiLenses ''Cursor
makeNakadiLenses ''DataChangeEvent
makeNakadiLenses ''DataChangeEventEnriched
makeNakadiLenses ''SubscriptionEventStreamBatch
makeNakadiLenses ''EventMetadata
makeNakadiLenses ''EventMetadataEnriched
makeNakadiLenses ''Partition
makeNakadiLenses ''CursorDistanceQuery
makeNakadiLenses ''CursorDistanceResult
makeNakadiLenses ''Timestamp
makeNakadiLenses ''SubscriptionEventStream
makeNakadiLenses ''EventTypeSchema
makeNakadiLenses ''EventType
makeNakadiLenses ''EventTypeSchemasResponse
makeNakadiLenses ''PaginationLink
makeNakadiLenses ''PaginationLinks
makeNakadiLenses ''SubscriptionEventTypeStatsResult
makeNakadiLenses ''ConsumeParameters
makeNakadiLenses ''SubscriptionCursorCommit
makeNakadiLenses ''SubscriptionCursor
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
