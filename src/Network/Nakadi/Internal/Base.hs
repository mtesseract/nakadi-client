module Network.Nakadi.Internal.Base
  ( module Network.Nakadi.Internal.Types.Base
  , runNakadiWithBase
  ) where

import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Types.Base

-- | This is a convenience function, which does `runNakadiT` and
-- 'runNakadiBaseT' at once.
runNakadiWithBase
  :: Config b -> NakadiT b (NakadiBaseT m) a -> m a
runNakadiWithBase config =
  runNakadiBaseT . runNakadiT config
