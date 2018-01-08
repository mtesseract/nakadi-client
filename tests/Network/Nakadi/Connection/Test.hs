{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Connection.Test where

import           ClassyPrelude
import           Network.HTTP.Client      (HttpException (..),
                                           HttpExceptionContent (..),
                                           Request (..), parseRequest,
                                           responseTimeoutMicro)
import           Network.HTTP.Types
import           Network.Nakadi
import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

testConnection :: TestTree
testConnection = testGroup "Connection"
  [ testCase "SimpleRetry" testSimpleRetry
  , testCase "ResponseTimeout Success" testResponseTimeoutSuccess
  , testCase "ResponseTimeout Fail" testResponseTimeoutFail
  ]

{-# NOINLINE requestCounter #-}
requestCounter :: TVar Int
requestCounter = unsafePerformIO $ newTVarIO 0

testServerRetryPort :: Port
testServerRetryPort = 5001

testServerResponseTimeoutPort :: Port
testServerResponseTimeoutPort = 5002

{-# NOINLINE testServerRequest #-}
testServerRequest :: Request
testServerRequest = unsafePerformIO $ parseRequest "http://localhost"

testServerRetryApp ::
  Int
  -> Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
testServerRetryApp noOfFailures req respond =
  case Wai.rawPathInfo req of
    "/event-types" -> do
      reqIdx <- atomically $ do
        modifyTVar requestCounter (+ 1)
        readTVar requestCounter
      let response = if reqIdx <= noOfFailures
                     then Wai.responseLBS status503 [] ""
                     else Wai.responseLBS status200 [] "[]"
      respond response
    _ ->
      respond $ Wai.responseLBS status404 [] ""

testServerResponseTimeoutApp ::
  Wai.Request
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
testServerResponseTimeoutApp req respond =
  case Wai.rawPathInfo req of
    "/event-types" -> do
      threadDelay (4 * 10^6) -- Wait for 4s
      respond $ Wai.responseLBS status200 [] "[]"
    _ ->
      respond $ Wai.responseLBS status404 [] ""

testSimpleRetry :: Assertion
testSimpleRetry = do
  conf :: ConfigIO <- newConfig Nothing testServerRequest { port = testServerRetryPort }
  withAsync (run testServerRetryPort (testServerRetryApp 1)) $ \_serverHandle -> do
    events <- runNakadiT conf eventTypesList
    [] @=? events

testResponseTimeoutSuccess :: Assertion
testResponseTimeoutSuccess = do
  let timeout = responseTimeoutMicro (5 * 10^6) -- Accept delay of 5s
      request = testServerRequest { port = testServerResponseTimeoutPort
                                  , responseTimeout = timeout }
  conf :: ConfigIO <- newConfig Nothing request
  withAsync (run testServerResponseTimeoutPort testServerResponseTimeoutApp) $ \_serverHandle -> do
    events <- runNakadiT conf eventTypesList
    [] @=? events

testResponseTimeoutFail :: Assertion
testResponseTimeoutFail = do
  res <- try $ do
    let timeout = responseTimeoutMicro (3 * 10^6) -- Accept delay of 3s
        request = testServerRequest { port = testServerResponseTimeoutPort
                                    , responseTimeout = timeout }
    conf :: ConfigIO <- newConfig Nothing request
    withAsync (run testServerResponseTimeoutPort testServerResponseTimeoutApp) $ \_serverHandle ->
      runNakadiT conf eventTypesList
  case res of
    Left (HttpExceptionRequest _request ResponseTimeout) -> return ()
    _ -> assertFailure "Expected HttpExceptionRequest with content ResponseTimeout"
