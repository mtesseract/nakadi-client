-- | Defines the LogFunc type.

module Network.Nakadi.Internal.Types.Logger where

import           Control.Monad.Logger
import           Prelude

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
