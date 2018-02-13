{-|
Module      : Network.Nakadi.Prelude
Description : Nakadi Client Library
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Convenience exports that should be fine to import unqualified.
-}

module Network.Nakadi.Prelude
  ( MonadNakadi(..)
  , MonadNakadiBase(..)
  , HasNakadiConfig(..)
  , NakadiT
  , NakadiBaseT
  , NakadiException
  , runNakadiT
  , runNakadiBaseT
  ) where

import           Network.Nakadi
