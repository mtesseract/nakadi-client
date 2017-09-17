module Network.Nakadi.Internal.Types.Subscription where

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Types.Service

-- | This context is required in the environment for running a
-- subscription. It is managed by the library, not by the user.

data SubscriptionEventStreamContext = SubscriptionEventStreamContext
  { _streamId       :: StreamId
  , _subscriptionId :: SubscriptionId
  , _ctxConfig      :: Config
  }
