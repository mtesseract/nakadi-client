-- | Internal Types

module Network.Nakadi.Internal.Types
  ( SubscriptionEventStreamContext(..)
  ) where

import           Network.Nakadi.Types.Config
import           Network.Nakadi.Types.Service

-- | SubscriptionEventStreamContext

data SubscriptionEventStreamContext = SubscriptionEventStreamContext
  { _streamId       :: StreamId
  , _subscriptionId :: SubscriptionId
  , _config         :: Config
  }
