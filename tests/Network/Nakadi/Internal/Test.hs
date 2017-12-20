module Network.Nakadi.Internal.Test where

import           Network.Nakadi.Internal.Http.Test
import           Network.Nakadi.Internal.Retry.Test
import           Network.Nakadi.Internal.Types.Test
import           Test.Tasty

testInternal :: TestTree
testInternal = testGroup "Internal"
  [ testTypes
  , testHttp
  , testRetry
  ]
