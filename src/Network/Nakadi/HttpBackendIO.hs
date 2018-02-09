{-|
Module      : Network.Nakadi.EventHttpBackendIO
Description : Exports IO based HTTP Backend
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module exports the standard IO based Nakadi HTTP Backend to make
it part of the public API.
-}

module Network.Nakadi.HttpBackendIO
  ( httpBackendIO
  ) where

import           Network.Nakadi.Internal.HttpBackendIO
