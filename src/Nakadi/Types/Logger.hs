-- | Defines the LogFunc type.

module Nakadi.Types.Logger where

import           Control.Monad.Logger
import           Prelude

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
