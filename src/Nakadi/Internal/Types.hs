-- | Internal Types

module Nakadi.Internal.Types
  ( SubscriptionEventStreamContext(..)
  ) where

import           Nakadi.Types.Config
import           Nakadi.Types.Service

-- | SubscriptionEventStreamContext

data SubscriptionEventStreamContext = SubscriptionEventStreamContext
  { _streamId       :: StreamId
  , _subscriptionId :: SubscriptionId
  , _config         :: Config
  }
