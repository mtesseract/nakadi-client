module Network.Nakadi.Internal.Test where

import Test.Tasty
import Network.Nakadi.Internal.Types.Test

testInternal :: TestTree
testInternal = testGroup "Internal"
  [ testTypes
  ]
