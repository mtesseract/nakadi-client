{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Internal.BackendIO where

import           Network.Nakadi.Internal.Prelude

import           Network.Nakadi.Internal.Types.Config

import qualified Data.ByteString.Lazy                 as LB
import qualified Network.HTTP.Client                  as HTTP

-- | Default 'HttpBackend' doing IO using http-client.
backendIO :: HTTP.Manager -> HttpBackend
backendIO mngr =
  HttpBackend { _httpLbs                        = httpLbs mngr
              , _responseOpen                   = responseOpen mngr
              , _responseClose                  = responseClose
              }
httpLbs :: MonadIO m => HTTP.Manager -> HTTP.Request -> m (Response LB.ByteString)
httpLbs mngr req = liftIO $ HTTP.httpLbs req mngr

responseOpen :: MonadIO m => HTTP.Manager -> HTTP.Request -> m (HTTP.Response HTTP.BodyReader)
responseOpen mngr req = liftIO $ HTTP.responseOpen req mngr

responseClose :: MonadIO m => HTTP.Response (HTTP.BodyReader) -> m ()
responseClose res = liftIO $ HTTP.responseClose res
