{-|
Module      : Network.Nakadi.Internal.Retry
Description : Nakadi Client Retry Mechanism
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module provides the basic retry mechanism via the retry package.
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Internal.Retry where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Retry
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Internal.Types

-- | Try to execute the provided IO action using the provided retry
-- policy. If executing the IO action raises specific exceptions of
-- type 'HttpException', the action will be potentially retried
-- (depending on the retry policy).
retryAction' :: (MonadIO m, MonadMask m) => RetryPolicyM IO -> m a -> m a
retryAction' policy ma =
  let nakadiRetryPolicy = RetryPolicyM $ \retryStatus ->
        liftIO (getRetryPolicyM policy retryStatus)
      handlerHttpWrapped = const $ Handler handlerHttp
  in recovering nakadiRetryPolicy [handlerHttpWrapped] (const ma)

  where handlerHttp (HttpExceptionRequest _ exceptionContent) =
          case exceptionContent of
            StatusCodeException response _ ->
              return $ responseStatus response `elem` [status500, status503]
            ResponseTimeout ->
              return True
            ConnectionTimeout ->
              return True
            ConnectionFailure _ ->
              return True
            InternalException _ ->
              return True
            ConnectionClosed ->
              return True
            _ ->
              return False

        handlerHttp _ = return False

-- | Try to execute the provided IO action, using the retry policy
-- from the provided configuration.
retryAction :: (MonadIO m, MonadMask m) => Config -> m a -> m a
retryAction config = retryAction' (config^.L.retryPolicy)
