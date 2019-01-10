{-|
Module      : Network.Nakadi.Types.Subscriptions
Description : Nakadi Service Subscription Types
Copyright   : (c) Moritz Clasmeier 2018, 2019
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module exposes types, which are related to consumption of
subscriptions, which are not modelled after the Nakadi API.
-}

module Network.Nakadi.Types.Subscriptions
  ( CommitStrategy(..)
  , CommitBufferingStrategy(..)
  , CommitTimeout(..)
  )
where

import           Network.Nakadi.Internal.Types.Subscriptions
