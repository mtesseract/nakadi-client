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

import           Data.Int


-- | This type encodes the supported strategies for subscription
-- cursor committing.
data CommitStrategy
  = CommitSync                          -- ^ This strategy synchronously commits every
                                        -- cursor.
  | CommitAsync CommitBufferingStrategy -- ^ This strategy sends cursors to be
                                        -- committed to a dedicated thread
                                        -- responsible for committing them. Cursors
                                        -- are commited one by one, without special
                                        -- buffering logic.

-- | This type encodes the supported buffering strategies for
-- asynchronous subscription cursor committing.
data CommitBufferingStrategy
  = CommitNoBuffer         -- ^ No buffering at all.
  | CommitTimeBuffer Int32 -- ^ Buffer for the specified duration,
                           -- given in milliseconds.
  | CommitSmartBuffer      -- ^ Buffer for a fixed duration, but
                           -- committing cursors immediately if the
                           -- number of events processed since the
                           -- last commit crosses a threshold derived
                           -- from @maxUncommittedEvents@.

data WorkerConcurrencyStrategy
  = WorkerConcurrencyNone      -- ^ Just one worker
  | WorkerConcurrencyFixed Int -- ^ Fixed number of workers
