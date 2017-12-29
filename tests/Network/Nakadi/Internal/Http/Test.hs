{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Internal.Http.Test
  ( testHttp
  ) where

import           ClassyPrelude
import           Conduit
import           Control.Monad
import           Control.Monad.Reader
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (CookieJar (..), Request (..),
                                               Response (..),
                                               ResponseClose (..))
import           Network.HTTP.Types
import           Network.Nakadi
import           Network.Nakadi.BackendIO
import           Network.Nakadi.Internal.Http
import           Test.Tasty
import           Test.Tasty.HUnit

testHttp :: TestTree
testHttp = testGroup "Http"
  [ testCase "HttpRequestModifier" testHttpRequestModifier
  ]

resp :: Response (IO ByteString)
resp = Response
    { responseStatus = status200
    , responseVersion = http11
    , responseHeaders = []
    , responseBody = pure ""
    , responseCookieJar = CJ []
    , responseClose' = ResponseClose (pure ())
    }

headers :: RequestHeaders
headers = [("test-header", "header-value")]

dummyResponseOpen :: Request -> IO (Response (IO ByteString))
dummyResponseOpen Request { .. } = do
  requestHeaders @=? headers
  pure resp

dummyHttpBackend :: MonadIO m => m HttpBackend
dummyHttpBackend = do
  mngr <- liftIO $ newManager defaultManagerSettings
  let backend = backendIO mngr
  pure $ backend { _responseOpen = dummyResponseOpen }

dummyRequestModifier :: Request -> IO Request
dummyRequestModifier request = pure (request { requestHeaders = headers })

testHttpRequestModifier :: Assertion
testHttpRequestModifier = do
    conf <- newConfig Nothing defaultRequest
    backend <- dummyHttpBackend
    let config = conf {_http = backend, _requestModifier = dummyRequestModifier }
    (_, source) <- runResourceT $ httpJsonBodyStream config ok200 (const (Right ())) [] id
    (_ :: Maybe Text) <- runResourceT $ runReaderT (runConduit $ source .| headC) ()
    return ()
