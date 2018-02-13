{-|
Module      : Network.Nakadi.Registry
Description : Implementation of Nakadi Registry API
Copyright   : (c) Moritz Schulte 2017, 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the @\/registry@ API.
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Registry
  ( registryPartitionStrategies
  ) where

import           Network.Nakadi.Internal.Http
import           Network.Nakadi.Internal.Prelude

path :: ByteString
path = "/registry"

-- | Retrieves supported partitioning strategies from Nakadi. @GET@ to
-- @\/registry\/partition-strategies@.
registryPartitionStrategies :: MonadNakadi r m => m [PartitionStrategy]
registryPartitionStrategies = httpJsonBody
  status200
  []
  (setRequestMethod "GET" . setRequestPath (path <> "/partition-strategies"))
