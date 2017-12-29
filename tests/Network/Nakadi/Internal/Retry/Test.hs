{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Internal.Retry.Test
  ( testRetry
  ) where

import           ClassyPrelude
import           Control.Lens
import           Control.Retry
import qualified Data.ByteString.Lazy          as LB
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal  (CookieJar (..), Request (..),
                                                Response (..),
                                                ResponseClose (..))
import           Network.HTTP.Types
import           Network.Nakadi
import           Network.Nakadi.Internal.Retry
import           Test.Tasty
import           Test.Tasty.HUnit

testRetry :: TestTree
testRetry = testGroup "Retry"
  [ testCase "HttpErrorCallback not called on success" testHttpErrorCallback0
  , testCase "HttpErrorCallback called on failure (n = 1)" testHttpErrorCallback1
  , testCase "HttpErrorCallback called on failure (n = maxRetries)" testHttpErrorCallbackMax
  , testCase "HttpErrorCallback called on failure (n = maxRetries + 1)" testHttpErrorCallbackMaxPlusOne
  ]

prepareMockResponse ::
  Monoid a
  => Int -- ^ Fail this many times prior to success
  -> IO (IO (Response a))
prepareMockResponse successNum = do
  counter <- newTVarIO 0
  return $ do
    current <- atomically $ do
      n <- readTVar counter
      modifyTVar counter (+ 1)
      return n
    if current >= successNum
    then return responseSuccess
    else throwM (HttpExceptionRequest defaultRequest (StatusCodeException responseFailure mempty))

  where responseTemplate = Response
          { responseStatus    = status200
          , responseVersion   = http11
          , responseHeaders   = []
          , responseBody      = mempty
          , responseCookieJar = CJ []
          , responseClose'    = ResponseClose (pure ())
          }
        responseSuccess = responseTemplate
        responseFailure = void $ responseTemplate { responseStatus = status503 }

maxRetries :: Int
maxRetries = 7

httpErrorCallback :: TVar Int -> Request -> HttpException -> RetryStatus -> Bool -> IO ()
httpErrorCallback tvCounter _request _exn _retryStatus finalFailure = do
  current <- atomically $ do
    n <- readTVar tvCounter
    modifyTVar tvCounter (+ 1)
    return n
  if current == maxRetries
     then True @=? finalFailure
     else False @=? finalFailure

mockHttpBackend :: Int -> IO HttpBackend
mockHttpBackend numFailures = do
  responder <- prepareMockResponse numFailures
  return HttpBackend
    { _httpLbs       = mockHttpLbs responder
    , _responseOpen  = (\req -> error "Not Implemented")
    , _responseClose = (\res -> error "Not Implemented")
    }

    where mockHttpLbs responder _request = responder

-- | Tests that the callback is called exactly numFailures times
-- before the request succeeds — depending on the retry policy.
testHttpErrorCallbackN :: Int -> Assertion
testHttpErrorCallbackN numFailures = do
  httpBackend <- mockHttpBackend numFailures
  counter <- newTVarIO 0
  conf <- newConfig Nothing defaultRequest
          <&> setHttpErrorCallback (httpErrorCallback counter)
          <&> setRetryPolicy (fullJitterBackoff 2 ++ limitRetries maxRetries)
  _response :: Either HttpException (Response LB.ByteString) <- try $
    retryAction conf defaultRequest (_httpLbs httpBackend)
  current <- readTVarIO counter
  numFailures @=? current

testHttpErrorCallback0 :: Assertion
testHttpErrorCallback0 = testHttpErrorCallbackN 0

testHttpErrorCallback1 :: Assertion
testHttpErrorCallback1 = testHttpErrorCallbackN 1

testHttpErrorCallbackMax :: Assertion
testHttpErrorCallbackMax = testHttpErrorCallbackN maxRetries

testHttpErrorCallbackMaxPlusOne :: Assertion
testHttpErrorCallbackMaxPlusOne = testHttpErrorCallbackN (maxRetries + 1)
