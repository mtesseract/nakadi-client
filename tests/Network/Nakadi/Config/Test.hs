{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.Nakadi.Config.Test where

import           ClassyPrelude               hiding (catch, throwM)
import           Control.Monad.Catch         (MonadCatch (..), MonadThrow (..))
import           Network.HTTP.Client
import           Network.Nakadi
import           Network.Nakadi.Tests.Common
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

testConfig :: TestTree
testConfig = testGroup "Config"
  [ testCase "Use Custom HttpBackend" testCustomHttpBackend ]

{-# NOINLINE requestsExecuted #-}
requestsExecuted :: TVar [Request]
requestsExecuted = unsafePerformIO . newTVarIO $ []

newtype MockNakadiT m a = MockNakadiT { runMockNakadiT :: m a
                                      } deriving (Functor, Applicative, Monad)

instance MonadTrans MockNakadiT where
  lift m = MockNakadiT m

instance MonadThrow m => MonadThrow (MockNakadiT m) where
  throwM = MockNakadiT . throwM

instance MonadCatch m => MonadCatch (MockNakadiT m) where
  catch (MockNakadiT m) c = MockNakadiT $ m `catch` \e -> runMockNakadiT (c e)

instance MonadIO m => MonadIO (MockNakadiT m) where
  liftIO = lift . liftIO

instance MonadNakadiHttp App (MockNakadiT App) where
  type NakadiHttpConstraint (MockNakadiT App) = MonadThrow (MockNakadiT App)
  nakadiHttpLbs _conf req _mngr = do
    atomically $ modifyTVar requestsExecuted (req :)
    throwM (HttpExceptionRequest req ResponseTimeout)

instance MonadNakadi App (MockNakadiT App) where
  nakadiAsk = pure mockConfig
  nakadiLocal _ ma = ma
  nakadiLift = lift

mockConfig :: Config App
mockConfig = unsafePerformIO $ newConfig Nothing defaultRequest

testCustomHttpBackend :: Assertion
testCustomHttpBackend = runApp $ do
  res0 <- try $ runMockNakadiT $
    registryPartitionStrategies -- This uses httpLbs.
  liftIO $ case res0 of
    Left (HttpExceptionRequest _ ResponseTimeout) -> return ()
    _ -> assertFailure "Expected ResponseTimeout exception from dummy HttpBackend"
  requests <- atomically . readTVar $ requestsExecuted
  liftIO $ 1 @=? length requests
