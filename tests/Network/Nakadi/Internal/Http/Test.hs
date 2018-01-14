{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Nakadi.Internal.Http.Test
  ( testHttp
  ) where

import           ClassyPrelude
import           Control.Lens
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.Nakadi
import           Network.Nakadi.Internal.Http
import           Test.Tasty
import           Test.Tasty.HUnit

testHttp :: TestTree
testHttp = testGroup "Http"
  [ testCase "HttpRequestModifier" testHttpRequestModifier
  ]

headers :: RequestHeaders
headers = [("test-header", "header-value")]

dummyRequestModifier :: Request -> IO Request
dummyRequestModifier request = pure (request { requestHeaders = headers })

testHttpRequestModifier :: Assertion
testHttpRequestModifier = do
  conf <- newConfig Nothing defaultRequest
          <&> setRequestModifier dummyRequestModifier
  request <- runNakadiT conf $ httpBuildRequest id
  requestHeaders request @=? headers
