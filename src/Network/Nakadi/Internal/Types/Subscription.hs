{-|
Module      : Network.Nakadi.Internal.Types.Subscription
Description : Nakadi Client Subscription Types (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal Subscription specific types, which are not part of the Nakadi
Service API but custom to this package.
-}

{-# LANGUAGE StrictData #-}

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
