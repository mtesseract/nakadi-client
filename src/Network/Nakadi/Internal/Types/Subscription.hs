module Network.Nakadi.Internal.Types.Subscription where

-- | SubscriptionEventStreamContext

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Service

data SubscriptionEventStreamContext = SubscriptionEventStreamContext
  { _streamId       :: StreamId
  , _subscriptionId :: SubscriptionId
  , _config         :: Config
  }
