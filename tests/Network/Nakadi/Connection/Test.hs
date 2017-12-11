module Network.Nakadi.Connection.Test where

import ClassyPrelude
import Network.Wai.Handler.Warp
import qualified Network.Wai as Wai
import Network.HTTP.Types
import Network.HTTP.Client ( Request(..)
                           , HttpException(..)
                           , HttpExceptionContent(..)
                           , responseTimeoutMicro
                           , parseRequest)
import           Network.Nakadi
import           Test.Tasty
import           Test.Tasty.HUnit
import System.IO.Unsafe

testConnection :: TestTree
testConnection = testGroup "Connection.Retry"
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
  conf <- newConfig Nothing testServerRequest { port = testServerRetryPort }
  withAsync (run testServerRetryPort (testServerRetryApp 2)) $ \_serverHandle -> do
    events <- eventTypesList conf
    [] @=? events

testResponseTimeoutSuccess :: Assertion
testResponseTimeoutSuccess = do
  let timeout = responseTimeoutMicro (5 * 10^6) -- Accept delay of 5s
  conf <- newConfig Nothing testServerRequest { port = testServerResponseTimeoutPort
                                              , responseTimeout = timeout }
  withAsync (run testServerResponseTimeoutPort testServerResponseTimeoutApp) $ \_serverHandle -> do
    events <- eventTypesList conf
    [] @=? events

testResponseTimeoutFail :: Assertion
testResponseTimeoutFail = do
  res <- try $ do
    let timeout = responseTimeoutMicro (3 * 10^6) -- Accept delay of 3s
    conf <- newConfig Nothing testServerRequest { port = testServerResponseTimeoutPort
                                                , responseTimeout = timeout }
    withAsync (run testServerResponseTimeoutPort testServerResponseTimeoutApp) $ \_serverHandle -> do
      eventTypesList conf
  case res of
    Left (HttpExceptionRequest _request ResponseTimeout) -> return ()
    _ -> assertFailure "Expected HttpExceptionRequest with content ResponseTimeout"
