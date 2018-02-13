{-|
Module      : Network.Nakadi.EventHttpBackendIO
Description : Implements IO based HTTP Backend
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

This module implements the standard IO based Nakadi HTTP Backend.
Useful, in case you just want to overwrite a subset of implementations
of the standard backend.
-}

module Network.Nakadi.Internal.HttpBackendIO where

import           Control.Lens                         ((<&>))
import qualified Data.ByteString.Lazy                 as LB
import           Data.Conduit                         (ConduitM, transPipe)
import           Network.HTTP.Client                  (Manager, Request,
                                                       Response)
import qualified Network.HTTP.Client                  as HTTP (httpLbs,
                                                               responseClose,
                                                               responseOpen)
import           Network.HTTP.Client.Conduit          (bodyReaderSource)
import           Network.HTTP.Client.TLS              (getGlobalManager)
import           Network.Nakadi.Internal.Prelude
import           Network.Nakadi.Internal.Retry
import           Network.Nakadi.Internal.Types.Config

getHttpManager
  :: MonadIO m
  => Maybe Manager
  -> m Manager
getHttpManager Nothing        = liftIO getGlobalManager
getHttpManager (Just manager) = pure manager

httpBackendIO
  :: (MonadMask b, MonadIO b)
  => HttpBackend b
httpBackendIO = HttpBackend
  { _httpLbs = httpLbs
  , _httpResponseOpen = responseOpen
  , _httpResponseClose = responseClose
  }

responseOpen
  :: MonadIO b
  => Config b
  -> Request
  -> Maybe Manager
  -> b (Response (ConduitM () ByteString b ()))
responseOpen _config req maybeMngr = do
  mngr <- getHttpManager maybeMngr
  liftIO $ HTTP.responseOpen req mngr <&> fmap (transPipe liftIO . bodyReaderSource)

responseClose
  :: MonadIO b
  => Response ()
  -> b ()
responseClose = liftIO . HTTP.responseClose

httpLbs
  :: (MonadMask b, MonadIO b)
  => Config b
  -> Request
  -> Maybe Manager
  -> b (Response LB.ByteString)
httpLbs config req maybeMngr = do
  mngr <- getHttpManager maybeMngr
  retryAction config req (\r -> liftIO $ HTTP.httpLbs r mngr)
