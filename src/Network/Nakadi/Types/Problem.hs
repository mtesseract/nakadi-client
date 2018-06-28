{-|
Module      : Network.Nakadi.Types.Problem
Description : Nakadi Problem Type
Copyright   : (c) Moritz Clasmeier 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module provides the Nakadi Problem Type.
-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Network.Nakadi.Types.Problem
  ( Problem(..)
  ) where

import           Network.Nakadi.Internal.Types.Problem
