{-|
Module      : Network.Nakadi.Types.Subscriptions
Description : Nakadi Service Types
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module defines types, which are related to consumption of
subscriptions, which are not modelled after the Nakadi API.
-}

module Network.Nakadi.Internal.Types.Subscriptions where

-- | This type encodes the supported strategies for subscription
-- cursor committing.
data CommitStrategy
  = CommitSyncUnbuffered -- ^ This strategy synchronously commit every
                         -- cursor.
