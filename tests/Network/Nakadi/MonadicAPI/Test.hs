{-# LANGUAGE FlexibleContexts #-}

module Network.Nakadi.MonadicAPI.Test where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Network.HTTP.Client
import qualified Network.Nakadi               as Nakadi
import           Network.Nakadi.Base
import qualified Network.Nakadi.Lenses        as L
import           Network.Nakadi.Tests.Common
import           Test.Tasty
import           Test.Tasty.HUnit

httpErrorCallback :: Request -> HttpException -> RetryStatus -> Bool -> App ()
httpErrorCallback _req exn _retryStatus _isFinalFailure =
  putStrLn $ "nakadi-client triggered exception: " <> tshow exn

testMonadicAPI :: Nakadi.Config App -> TestTree
testMonadicAPI conf = testGroup "Monadic API"
  [ testCase "Simple Monadic API" (testSimpleMonadicAPI conf')
  , testCase "Simple NakadiBaseT" (testSimpleNakadiBaseT confLifted)
  , testCase "NakadiBaseT with non-trivial stack" (testNonTrivNakadiBaseT confLifted)
  ]

  where conf' = Nakadi.setHttpErrorCallback httpErrorCallback conf
        confLifted = Nakadi.newConfig Nakadi.httpBackendIO (conf^.L.requestTemplate)

testSimpleMonadicAPI :: Nakadi.Config App -> Assertion
testSimpleMonadicAPI conf = runApp . Nakadi.runNakadiT conf $ do
  -- Tests Nakadi call within NakadiT monad transformer
  _events <- Nakadi.eventTypesList
  -- But we can also call Nakadi from within monad transformers on top of NakadiT:
  flip runReaderT () $ do
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
