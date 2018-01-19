{-|
Module      : Network.Nakadi.Internal.Types.Logger
Description : Nakadi Client Logger Types (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal logging specific types.
-}

module Network.Nakadi.Internal.Types.Logger where

import           Control.Monad.Logger
import           Prelude

-- | Type of a logger callback provided to nakadi-client for logging
-- purposes.
type LogFunc m = Loc -> LogSource -> LogLevel -> LogStr -> m ()

-- | 'LogFunc' specialized to IO.
type LogFuncIO = LogFunc IO
