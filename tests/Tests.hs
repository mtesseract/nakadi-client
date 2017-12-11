import           ClassyPrelude

import           Network.HTTP.Client
import           Network.Nakadi
import           Network.Nakadi.Connection.Test
import           Network.Nakadi.EventTypes.Test
import           Network.Nakadi.Registry.Test
import           Network.Nakadi.Subscriptions.Test
import           System.Environment
import           Test.Tasty

createConfig :: IO Config
createConfig = do
  nakadiEndpoint <- fromMaybe (error "TEST_NAKADI_ENDPOINT not set") <$>
                    lookupEnv "TEST_NAKADI_ENDPOINT"
  request <- parseRequest nakadiEndpoint
  newConfig Nothing request

main :: IO ()
main = do
  conf <- createConfig
  defaultMain (tests conf)

tests :: Config -> TestTree
tests conf = testGroup "Test Suite"
  [ testConnection
  , testEventTypes conf
  , testRegistry conf
  , testSubscriptions conf
  ]
