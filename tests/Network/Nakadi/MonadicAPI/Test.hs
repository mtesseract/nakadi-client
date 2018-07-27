{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Network.Nakadi.MonadicAPI.Test where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Network.HTTP.Client
import qualified Network.Nakadi                as Nakadi
import           Network.Nakadi.Base
import qualified Network.Nakadi.Lenses         as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           System.Environment
import qualified Network.Nakadi.Unsafe.IO      as Nakadi
import           Test.Tasty.HUnit

httpErrorCallback :: Request -> HttpException -> RetryStatus -> Bool -> App ()
httpErrorCallback _req exn _retryStatus _isFinalFailure =
  putStrLn $ "nakadi-client triggered exception: " <> tshow exn

testMonadicAPI :: Nakadi.Config App -> TestTree
testMonadicAPI conf = testGroup
  "Monadic API"
  [ testCase "Simple Monadic API"                    (testSimpleMonadicAPI conf')
  , testCase "Simple NakadiBaseT"                    (testSimpleNakadiBaseT confLifted)
  , testCase "NakadiBaseT with non-trivial stack"    (testNonTrivNakadiBaseT confLifted)
  , testCase "MonadNakadi instance for ReaderT"      (testReaderT conf)
  , testCase "MonadNakadi instance for IO"           (testIO conf)
  , testCase "MonadNakadi instance for IO, from Env" (testIOFromEnv conf)
  ]
 where
  conf'      = Nakadi.setHttpErrorCallback httpErrorCallback conf
  confLifted = Nakadi.newConfig Nakadi.httpBackendIO (conf ^. L.requestTemplate)

testSimpleMonadicAPI :: Nakadi.Config App -> Assertion
testSimpleMonadicAPI conf =
  runApp
    . Nakadi.runNakadiT conf
    $ do
  -- Tests Nakadi call within NakadiT monad transformer
        _events <- Nakadi.eventTypesList
        -- But we can also call Nakadi from within monad transformers on top of NakadiT:
        void $ flip runStateT () $ do
          _events <- Nakadi.eventTypesList
          pure ()
        -- Required for consuming via subscription API is the ability to
        -- call Nakadi from within ResourceT:
        runResourceT $ do
          _partitionStrategies <- Nakadi.registryPartitionStrategies
          pure ()
        -- We can retrieve the configuration using nakadiAsk:
        _conf <- Nakadi.nakadiAsk
        pure ()

testSimpleNakadiBaseT :: Nakadi.Config (NakadiBaseT App) -> Assertion
testSimpleNakadiBaseT conf = runApp . Nakadi.runNakadiWithBase conf $ do
  _events <- Nakadi.eventTypesList
  pure ()

-- | This tests the verbose but more general method of seperating
-- runNakadiBaseT from runNakadiT, changing the monad transformer
-- stack inbetween (by adding NoLoggingT).
testNonTrivNakadiBaseT :: Nakadi.Config (NakadiBaseT App) -> Assertion
testNonTrivNakadiBaseT conf =
  runApp . Nakadi.runNakadiBaseT . runNoLoggingT . Nakadi.runNakadiT conf $ do
    _events <- Nakadi.eventTypesList
    pure ()


data Env = Env { nakadiConfiguration :: Nakadi.Config App }

instance Nakadi.HasNakadiConfig App Env where
  nakadiConfig = nakadiConfiguration

-- | This test uses the 'MonadNakadi' instance for 'ReaderT' with a
-- 'HasNakadiConfig' constraint.
testReaderT :: Nakadi.Config App -> Assertion
testReaderT conf = runApp . flip runReaderT (Env conf) $ do
  _events <- Nakadi.eventTypesList
  pure ()

-- | Test the 'MonadNakadi' instance for 'IO' global configuration
-- initialized from the environment (environment variable @NAKADI_URL@).
testIOFromEnv :: Nakadi.Config App -> Assertion
testIOFromEnv conf = flip finally (unsetEnv envNakadiUrl) $ do
  setEnv envNakadiUrl url
  Nakadi.initializeGlobalConfigurationFromEnv
  _eventsTypes <- Nakadi.eventTypesList
  pure ()
 where
  url          = show (getUri (conf ^. L.requestTemplate))
  envNakadiUrl = "NAKADI_URL"

-- | Test the 'MonadNakadi' instance for 'IO' using explicitely
-- set global configuration.
testIO :: Nakadi.Config App -> Assertion
testIO conf = do
  let confIO = Nakadi.newConfigIO (conf ^. L.requestTemplate)
  Nakadi.setGlobalConfiguration confIO
  _eventsTypes <- Nakadi.eventTypesList
  pure ()
