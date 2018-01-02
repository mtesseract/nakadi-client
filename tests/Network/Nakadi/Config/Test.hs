module Network.Nakadi.Config.Test where

import           ClassyPrelude
import           Control.Lens                ((<&>))
import           Control.Retry
import qualified Data.ByteString.Lazy        as LB
import           Network.HTTP.Client
import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

testConfig :: TestTree
testConfig = testGroup "Config"
  [ testCase "Use Custom HttpBackend" testCustomHttpBackend ]

{-# NOINLINE requestsExecuted #-}
requestsExecuted :: TVar [Request]
requestsExecuted = unsafePerformIO . newTVarIO $ []

dummyHttpLbs :: Request -> IO (Response LB.ByteString)
dummyHttpLbs req = do
  atomically $ modifyTVar requestsExecuted (req :)
  throwIO (HttpExceptionRequest req ResponseTimeout)

dummyResponseOpen :: Request -> IO (Response (IO ByteString))
dummyResponseOpen req = do
  atomically $ modifyTVar requestsExecuted (req :)
  throwIO (HttpExceptionRequest req ResponseTimeout)

dummyResponseClose :: Response BodyReader -> IO ()
dummyResponseClose _response = return ()

dummyHttpBackend :: HttpBackend
dummyHttpBackend = HttpBackend dummyHttpLbs dummyResponseOpen dummyResponseClose

testCustomHttpBackend :: Assertion
testCustomHttpBackend = runApp $ do
  let trivialRetryPolicy = limitRetries 0 :: RetryPolicyM App
  conf <- newConfig Nothing defaultRequest
          <&> setHttpBackend dummyHttpBackend
          <&> setRetryPolicy trivialRetryPolicy
  res0 <- runNakadiT conf $
    try $ registryPartitionStrategies conf -- This should use httpLbs
  liftIO $ case res0 of
    Left (HttpExceptionRequest _ ResponseTimeout) -> return ()
    _ -> assertFailure "Expected ResponseTimeout exception from dummy HttpBackend"
  requests <- atomically . readTVar $ requestsExecuted
  liftIO $ 1 @=? length requests
