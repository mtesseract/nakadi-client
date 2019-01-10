{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Nakadi.Internal.Http.Test
  ( testHttp
  )
where

import           ClassyPrelude
import           Control.Arrow
import           Control.Lens
import qualified Data.ByteString.Lazy          as LB
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.Nakadi
import           Network.Nakadi.Internal.Http
import qualified Network.Nakadi.Lenses         as L
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map                      as Map
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Prelude                        ( read )
import qualified Data.Text                     as Text
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)
import Conduit

testHttp :: TestTree
testHttp = testGroup
  "Http"
  [ testCase "HttpRequestModifier"        testHttpRequestModifier
  , testCase "FlowIdInclusion"            testFlowIdInclusion
  , testCase "FlowIdMissing"              testFlowIdMissing
  , testCase "FlowIdInclusionHttp"        testFlowIdInclusionHttp
  , testCase "FlowIdMissingHttp"          testFlowIdMissingHttp
  , testCase "CommitTimeoutInclusionHttp" testCommitTimeoutInclusionHttp
  , testCase "CommitTimeoutMissingHttp"   testCommitTimeoutMissingHttp
  ]

extractFlowId :: Request -> Maybe FlowId
extractFlowId =
  requestHeaders >>> filter (\(key, _) -> key == "X-Flow-Id") >>> listToMaybe >>> fmap
    (FlowId . decodeUtf8 . snd)

extractCommitTimeout :: Request -> Maybe CommitTimeout
extractCommitTimeout =
  queryString >>> parseSimpleQuery >>> Map.fromList >>> Map.lookup "commit_timeout" >>> fmap
    (CommitTimeout . read . Text.unpack . decodeUtf8)

headers :: RequestHeaders
headers = [("test-header", "header-value")]

dummyRequestModifier :: Request -> IO Request
dummyRequestModifier request = pure (request { requestHeaders = headers })

testHttpRequestModifier :: Assertion
testHttpRequestModifier = do
  let conf = newConfigIO defaultRequest & setRequestModifier dummyRequestModifier
  request <- runNakadiT conf $ httpBuildRequest id
  requestHeaders request @=? headers

testFlowIdInclusion :: Assertion
testFlowIdInclusion = do
  let flowId = FlowId "shalom"
      config = newConfigIO defaultRequest & setFlowId flowId
  Just flowId @=? (defaultRequest & (includeFlowId config >>> extractFlowId))

testFlowIdMissing :: Assertion
testFlowIdMissing = do
  let config = newConfigIO defaultRequest
  Nothing @=? (defaultRequest & (includeFlowId config >>> extractFlowId))

mockHttpLbs
  :: b ~ IO
  => TVar (Maybe Request)
  -> Config b
  -> Request
  -> Maybe Manager
  -> b (Response LB.ByteString)
mockHttpLbs tv _config request _manager = do
  liftIO . atomically $ writeTVar tv (Just request)
  throwString "Mock"

mockHttpResponseOpen
  :: b ~ IO
  => TVar (Maybe Request)
  -> Config b
  -> Request
  -> Maybe Manager
  -> b (Response (ConduitM () ByteString b ()))
mockHttpResponseOpen tv _config request _manager = do
  liftIO . atomically $ writeTVar tv (Just request)
  throwString "Mock"

mockHttpResponseClose
  :: b ~ IO
  => Response ()
  -> b ()
mockHttpResponseClose _ = pure ()

testFlowIdInclusionHttp :: Assertion
testFlowIdInclusionHttp = do
  tv <- atomically $ newTVar Nothing
  let flowId      = FlowId "shalom"
      httpBackend = httpBackendIO & L.httpLbs .~ mockHttpLbs tv
      config      = newConfig httpBackend defaultRequest & setFlowId flowId
  Left (StringException _ _) <- try $ runNakadiT config eventTypesList
  Just requestExecuted       <- liftIO . atomically $ readTVar tv
  Just flowId @=? extractFlowId requestExecuted

testFlowIdMissingHttp :: Assertion
testFlowIdMissingHttp = do
  tv <- atomically $ newTVar Nothing
  let httpBackend = httpBackendIO & L.httpLbs .~ mockHttpLbs tv
      config      = newConfig httpBackend defaultRequest
  Left (StringException _ _) <- try $ runNakadiT config eventTypesList
  Just requestExecuted       <- liftIO . atomically $ readTVar tv
  Nothing @=? extractFlowId requestExecuted

dummySubscriptionId :: SubscriptionId
dummySubscriptionId = SubscriptionId (fromJust (UUID.fromString "975F8AEE-1F22-4798-8864-CC418CDF66EB"))

testCommitTimeoutInclusionHttp :: Assertion
testCommitTimeoutInclusionHttp = do
  tv <- atomically $ newTVar Nothing
  let commitTimeout = CommitTimeout 42
      httpBackend   = httpBackendIO
                      & L.httpResponseOpen .~ mockHttpResponseOpen tv
                      & L.httpResponseClose .~ mockHttpResponseClose
      config        = newConfig httpBackend defaultRequest & setCommitTimeout commitTimeout
  Left (StringException _ _) <- try $ runResourceT . runNakadiT config $ subscriptionProcess
    dummySubscriptionId (\(_ :: SubscriptionEventStreamBatch Int) -> pure ())
  Just requestExecuted <- liftIO . atomically $ readTVar tv
  Just commitTimeout @=? extractCommitTimeout requestExecuted

testCommitTimeoutMissingHttp :: Assertion
testCommitTimeoutMissingHttp = do
  tv <- atomically $ newTVar Nothing
  let httpBackend   = httpBackendIO
                      & L.httpResponseOpen .~ mockHttpResponseOpen tv
                      & L.httpResponseClose .~ mockHttpResponseClose
      config        = newConfig httpBackend defaultRequest
  Left (StringException _ _) <- try $ runResourceT . runNakadiT config $ subscriptionProcess
    dummySubscriptionId (\(_ :: SubscriptionEventStreamBatch Int) -> pure ())
  Just requestExecuted       <- liftIO . atomically $ readTVar tv
  Nothing @=? extractCommitTimeout requestExecuted
