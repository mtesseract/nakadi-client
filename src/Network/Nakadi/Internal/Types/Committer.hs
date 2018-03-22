module Network.Nakadi.Internal.Types.Committer where

{-# LANGUAGE StrictData            #-}

import           Data.HashMap.Strict                   (HashMap)

import           Network.Nakadi.Internal.Types.Service

data StagedCursor a =
  StagedCursor { _cursor     :: SubscriptionCursor
               , _enrichment :: a }

newtype StagedCursors a = StagedCursors
  { _cursorsMap :: HashMap (EventTypeName, PartitionName) (StagedCursor a)
  }
