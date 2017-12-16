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

httpErrorCallback :: Config -> Request -> RetryStatus -> HttpException -> IO ()
httpErrorCallback config =
  case config^.L.httpErrorCallback of
    Just cb -> cb
    Nothing -> \_req _retryStatus _exn -> pure ()

-- | Try to execute the provided IO action using the provided retry
-- policy. If executing the IO action raises specific exceptions of
-- type 'HttpException', the action will be potentially retried
-- (depending on the retry policy).
retryAction ::
  (MonadIO m, MonadMask m)
  => Config
  -> Request
  -> (Request -> m a)
  -> m a
retryAction config req ma =
  let policy = config^.L.retryPolicy
      nakadiRetryPolicy = RetryPolicyM $ \retryStatus ->
        liftIO (getRetryPolicyM policy retryStatus)
  in recovering nakadiRetryPolicy [handlerHttp] (const (ma req))

  where handlerHttp retryStatus = Handler $ \exn -> do
          liftIO $ httpErrorCallback config req retryStatus exn
          pure $ shouldRetry exn

        shouldRetry (HttpExceptionRequest _ exceptionContent) =
          case exceptionContent of
            StatusCodeException response _ ->
              responseStatus response `elem` [status500, status503]
            ResponseTimeout ->
              True
            ConnectionTimeout ->
              True
            ConnectionFailure _ ->
              True
            InternalException _ ->
              True
            ConnectionClosed ->
              True
            _ ->
              False

        shouldRetry _ = False
