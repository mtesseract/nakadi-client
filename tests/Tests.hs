{-# LANGUAGE ScopedTypeVariables #-}
import           ClassyPrelude

import           Network.HTTP.Client
import           Network.Nakadi
import           Network.Nakadi.Config.Test
import           Network.Nakadi.Connection.Test
import           Network.Nakadi.EventTypes.Test
import           Network.Nakadi.Examples.Test
import           Network.Nakadi.Internal.Test
import           Network.Nakadi.MonadicAPI.Test
import           Network.Nakadi.Registry.Test
import           Network.Nakadi.Subscriptions.Test
import           Network.Nakadi.Tests.Common
import           System.Environment
import           System.Exit
import           System.IO                         (hFlush)
import           Test.Tasty

main :: IO ()
main = do
  putStrLn ""
  maybeNakadiEndpoint <- lookupEnv "TEST_NAKADI_ENDPOINT"
  (label, runTests) <- case maybeNakadiEndpoint of
    Nothing -> do
      hPut stderr . encodeUtf8 $
        "***************************************************************\n\
        \** The environment variable TEST_NAKADI_ENDPOINT is not set. **\n\
        \**                                                           **\n\
        \**              SKIPPING INTEGRATION TESTS                   **\n\
        \**                                                           **\n\
        \** If you want to run the integration tests contained in     **\n\
        \** this test suite, you need to run a Nakadi server which    **\n\
        \** allows access without authentication. Then, set the       **\n\
        \** environment variable TEST_NAKADI_ENDPOINT to the base     **\n\
        \** endpoint of this Nakadi server.                           **\n\
        \**                                                           **\n\
        \** To run a local Nakadi server clone the repository         **\n\
        \**                                                           **\n\
        \**     https://github.com/zalando/nakadi                     **\n\
        \**                                                           **\n\
        \** and follow the contained instructions; the command        **\n\
        \**                                                           **\n\
        \**     ./gradlew startNakadi                                 **\n\
        \**                                                           **\n\
        \** starts a local Nakadi server at http://localhost:8080.    **\n\
        \**                                                           **\n\
        \** Thus, set TEST_NAKADI_ENDPOINT to http://localhost:8080.  **\n\
        \***************************************************************\n"
      return ("nakadi-client Test Suite (w/o integration tests)", unitTests)
    Just nakadiEndpoint -> do
      let nakadiEndpointT = pack nakadiEndpoint
      case parseRequest nakadiEndpoint of
        Just request -> do
          let conf   = newConfig request
              confIO = newConfig request :: ConfigIO
          return ("nakadi-client Test Suite",
                  integrationTests conf confIO ++ unitTests)
        Nothing -> do
          hPut stderr . encodeUtf8 $
            "Failed to parse Nakadi URL in TEST_NAKADI_ENDPOINT (" <> nakadiEndpointT <> ")"
          hFlush stderr
          exitFailure
  defaultMain (testGroup label runTests)

unitTests :: [TestTree]
unitTests =
  [ testInternal
  , testConfig
  , testConnection
  ]

integrationTests :: Config App -> ConfigIO -> [TestTree]
integrationTests conf confIO =
  [ testExamples confIO
  , testEventTypes conf
  , testRegistry conf
  , testSubscriptions conf
  , testMonadicAPI conf
  ]
