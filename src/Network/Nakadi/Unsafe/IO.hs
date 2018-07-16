{-|
Module      : Network.Nakadi.Unsafe.IO
Description : Support for MonadNakadi IO Instance
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Nakadi-client provides a 'MonadNakadi' instance for the 'IO' monad
accessing a global mutable configuration.

This module exports the user-visible API for managing this global
configuration.
-}

module Network.Nakadi.Unsafe.IO
  ( initializeGlobalConfigurationFromEnv
  , setGlobalConfiguration
  , modifyGlobalConfiguration
  )
where

import           Network.Nakadi.Internal.Unsafe.IO
