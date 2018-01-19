{-# LANGUAGE FlexibleContexts #-}
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

{-# LANGUAGE LambdaCase       #-}

module Network.Nakadi.Internal.Retry
  ( retryAction
  ) where

import           Network.Nakadi.Internal.Prelude

import           Control.Lens
import           Control.Retry
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import qualified Network.Nakadi.Internal.Lenses       as L
import           Network.Nakadi.Internal.Types.Config

-- | Invokes the HTTP Error Callback set in the configuration for the
-- provided 'Request', 'HttpException' and 'RetryStatus'. If no
-- callback is set, this is no-op.
invokeHttpErrorCallback ::
  (MonadIO b)
  => Config b
  -> Request
  -> HttpException
  -> RetryStatus
  -> b ()
invokeHttpErrorCallback config req exn retryStatus =
  case config^.L.httpErrorCallback of
    Just cb -> do
      finalFailure <- isFinalFailure
      cb req exn retryStatus finalFailure
    Nothing -> pure ()

  where isFinalFailure = do
          let policy = liftIORetryPolicy (config^.L.retryPolicy)
          applyPolicy policy retryStatus >>= \case
            Just _  -> pure False
            Nothing -> pure True


liftIORetryPolicy :: MonadIO b => RetryPolicyM IO -> RetryPolicyM b
liftIORetryPolicy rp = RetryPolicyM $ liftIO . getRetryPolicyM rp

-- | Try to execute the provided IO action using the provided retry
-- policy. If executing the IO action raises specific exceptions of
-- type 'HttpException', the action will be potentially retried
-- (depending on the retry policy).
retryAction ::
  (MonadIO b, MonadMask b)
  => Config b
  -> Request
  -> (Request -> b a)
  -> b a
retryAction config req ma =
  let policy = liftIORetryPolicy (config^.L.retryPolicy)
  in recovering policy [handlerHttp] (\_retryStatus -> ma req)

  where handlerHttp retryStatus = Handler $ \exn -> do
          invokeHttpErrorCallback config req exn retryStatus
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
