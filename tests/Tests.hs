import           ClassyPrelude

import           Control.Lens
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

deserializationFailureCB :: ByteString -> IO ()
deserializationFailureCB event = do
  error $ "Failed to deserialize: " <> show event

main :: IO ()
main = do
  conf <- createConfig
          <&> setDeserializationFailureCallback deserializationFailureCB
  defaultMain (tests conf)

tests :: Config -> TestTree
tests conf = testGroup "Test Suite"
  [ testEventTypes conf
  ]
