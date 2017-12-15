module Network.Nakadi.Internal.Test where

import Test.Tasty
import Network.Nakadi.Internal.Types.Test
import Network.Nakadi.Internal.Http.Test

testInternal :: TestTree
testInternal = testGroup "Internal"
  [ testTypes
  , testHttp
  ]
