{-# LANGUAGE StrictData #-}

module Network.Nakadi.Internal.Types.Committer where

import           Data.Int
import           Data.HashMap.Strict            ( HashMap )

import           Network.Nakadi.Internal.Types.Service

data SubscriptionCursorWithCounter = SubscriptionCursorWithCounter { _cursor :: SubscriptionCursor, _nEvents :: Int }

type StagedCursorsMap a = HashMap (EventTypeName, PartitionName) a
