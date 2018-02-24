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

data CommitBufferingStrategy
  = CommitNoBuffer

-- | This type encodes the supported strategies for subscription
-- cursor committing.
data CommitStrategy
  = CommitSync                          -- ^ This strategy synchronously commit every
                                        -- cursor.
  | CommitAsync CommitBufferingStrategy -- ^ This strategy sends cursors to be
                                        -- committed to a dedicated thread
                                        -- responsible for committing them. Cursors
                                        -- are commited one by one, without special
                                        -- buffering logic.
