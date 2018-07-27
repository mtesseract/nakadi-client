{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Nakadi.Internal.GlobalConfig where

import           UnliftIO.STM

import           System.IO.Unsafe

import           Network.Nakadi.Internal.Types.Config
import           Network.Nakadi.Internal.Prelude

{-# NOINLINE globalConfiguration #-}
globalConfiguration :: TVar (Maybe ConfigIO)
globalConfiguration = unsafePerformIO $ newTVarIO Nothing
