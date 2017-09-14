import           Prelude

import           Data.Maybe
import           Network.HTTP.Client
import           Network.Nakadi
import           Network.Nakadi.EventTypes.Test
import           System.Environment
import           Test.Tasty

createConfig :: IO Config
createConfig = do
  nakadiEndpoint <- fromMaybe (error "NAKADI_ENDPOINT not set") <$>
                    lookupEnv "NAKADI_ENDPOINT"
  request <- parseRequest nakadiEndpoint
  newConfig Nothing request

main :: IO ()
main = do
  conf <- createConfig
  defaultMain (tests conf)

tests :: Config -> TestTree
tests conf = testGroup "Test Suite"
  [ testEventTypes conf
  ]