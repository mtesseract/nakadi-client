{-|
Module      : Network.Nakadi.Internal.Http
Description : Nakadi Client HTTP (Internal)
Copyright   : (c) Moritz Schulte 2017
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Internal module containing HTTP client relevant code.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Nakadi.Internal.Http
  ( module Network.HTTP.Simple
  , module Network.HTTP.Types.Status
  , module Network.Nakadi.Internal.Types
  , httpJsonBody
  , httpJsonNoBody
  , httpJsonBodyStream
  , errorClientNotAuthenticated
  , errorUnprocessableEntity
  , errorAccessForbidden
  , errorTooManyRequests
  , errorBadRequest
  , errorSubscriptionNotFound
  , errorCursorAlreadyCommitted
  , errorCursorResetInProgress
  , errorEventTypeNotFound
  , errorSubscriptionExistsAlready
  , errorBatchPartiallySubmitted
  , errorBatchNotSubmitted
  , setRequestQueryParameters
  ) where

import           Network.Nakadi.Internal.Prelude

import           Conduit
import           Control.Arrow
import           Control.Lens
import           Control.Monad                   (void)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy            as ByteString.Lazy
import qualified Data.Conduit.Binary             as Conduit
import qualified Data.Text                       as Text
import           Network.HTTP.Client             ( BodyReader
                                                 , checkResponse
                                                 , responseStatus
                                                 , HttpException(..)
                                                 , HttpExceptionContent(..))
import           Network.HTTP.Client.Conduit     (bodyReaderSource)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import qualified Network.Nakadi.Internal.Lenses  as L
import           Network.Nakadi.Internal.Retry
import           Network.Nakadi.Internal.Types
import           Network.Nakadi.Internal.Util

conduitDecode ::
  (MonadIO m, FromJSON a)
  => Config -- ^ Configuration, containing the
            -- deserialization-failure-callback.
  -> ConduitM ByteString a m () -- ^ Conduit deserializing bytestrings
                                -- into custom values
conduitDecode Config { .. } = awaitForever $ \a ->
  case eitherDecodeStrict a of
    Right v  -> yield v
    Left err -> liftIO $ callback a (Text.pack err)

    where callback = fromMaybe dummyCallback _deserializationFailureCallback
          dummyCallback _ _ = return ()

-- | Throw 'HttpException' exception on server errors (5xx).
checkNakadiResponse :: Request -> Response BodyReader -> IO ()
checkNakadiResponse request response =
  when (statusCode (responseStatus response) `div` 100 == 5) $
  throwIO $ HttpExceptionRequest request (StatusCodeException (void response) mempty)

httpBuildRequest ::
  (MonadIO m, MonadCatch m)
  => Config -- ^ Configuration, contains the impure request modifier
  -> (Request -> Request) -- ^ Pure request modifier
  -> m Request -- ^ Resulting request to execute
httpBuildRequest Config { .. } requestDef =
  let manager = _manager
      request = requestDef _requestTemplate
                   & setRequestManager manager
                   & (\req -> req { checkResponse = checkNakadiResponse })
  in modifyRequest _requestModifier request


-- | Modify the Request based on a user function in the configuration.
modifyRequest :: (MonadIO m, MonadCatch m) => (Request -> IO Request) -> Request -> m Request
modifyRequest rm request =
  tryAny (liftIO (rm request)) >>= \case
    Right modifiedRequest -> return modifiedRequest
    Left  exn             -> throwIO $ RequestModificationException exn

-- | Executes an HTTP request using the provided configuration and a
-- pure request modifier.
httpExecRequest ::
  (MonadIO m, MonadMask m)
  => Config
  -> (Request -> Request)
  -> m (Response ByteString.Lazy.ByteString)
httpExecRequest config requestDef =
  httpBuildRequest config requestDef
  >>= retryAction config . liftIO . (config^.L.http.L.httpLbs)

-- | Executes an HTTP request using the provided configuration and a
-- pure request modifier. Returns the HTTP response and separately the
-- response status.
httpExecRequestWithStatus ::
  (MonadIO m, MonadMask m)
  => Config -- ^ Configuration
  -> (Request -> Request) -- ^ Pure request modifier
  -> m (Response ByteString.Lazy.ByteString, Status)
httpExecRequestWithStatus config requestDef =
  (identity &&& getResponseStatus) <$> httpExecRequest config requestDef

httpJsonBody ::
  (MonadMask m, MonadIO m, FromJSON a)
  => Config
  -> Status
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> m a
httpJsonBody config successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus config requestDef
  if status == successStatus
    then decodeThrow (getResponseBody response)
    else case lookup status exceptionMap' of
           Just mkExn -> mkExn (getResponseBody response) >>= throwIO
           Nothing    -> throwIO (UnexpectedResponse (void response))

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

httpJsonNoBody ::
  (MonadMask m, MonadIO m)
  => Config
  -> Status
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> m ()
httpJsonNoBody config successStatus exceptionMap requestDef = do
  (response, status) <- httpExecRequestWithStatus config requestDef
  unless (status == successStatus) $
    case lookup status exceptionMap' of
      Just mkExn -> mkExn (getResponseBody response) >>= throwIO
      Nothing    -> throwIO (UnexpectedResponse (void response))

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

httpJsonBodyStream ::
  (MonadMask m, MonadIO m, FromJSON a, MonadBaseControl IO m, MonadResource m)
  => Config
  -> Status
  -> (Response () -> Either Text b)
  -> [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
  -> (Request -> Request)
  -> m (b, ConduitM () a (ReaderT r m) ())
httpJsonBodyStream config successStatus f exceptionMap requestDef = do
  let manager        = config^.L.manager
      request        = requestDef (config^.L.requestTemplate)
                         & setRequestManager manager
      responseOpen   = config^.L.http.L.responseOpen
      responseClose  = config^.L.http.L.responseClose
  modifiedRequest <- modifyRequest (config^.L.requestModifier) request
  (_, response) <- allocate (retryAction config (responseOpen modifiedRequest manager)) responseClose
  let response_  = void response
      bodySource = bodyReaderSource (getResponseBody response)
      status     = getResponseStatus response
  if status == successStatus
    then do connectCallback (config^.L.logFunc) response_
            let conduit = bodySource
                          .| Conduit.lines
                          .| conduitDecode config
            case f response_ of
              Left errMsg -> throwString (Text.unpack errMsg)
              Right b     -> return (b, readerC (const conduit))
    else case lookup status exceptionMap' of
           Just mkExn -> conduitDrainToLazyByteString bodySource >>= mkExn >>= throwIO
           Nothing    -> throwIO (UnexpectedResponse response_)

  where exceptionMap' = exceptionMap ++ defaultExceptionMap

        connectCallback = case config^.L.streamConnectCallback of
          Just cb -> \logFunc response -> liftIO $ cb logFunc response
          Nothing -> \_ _ -> return ()

setRequestQueryParameters ::
  [(ByteString, ByteString)]
  -> Request
  -> Request
setRequestQueryParameters parameters = setRequestQueryString parameters'
  where parameters' = map (fmap Just) parameters

defaultExceptionMap ::
  MonadThrow m
  => [(Status, ByteString.Lazy.ByteString -> m NakadiException)]
defaultExceptionMap =
  [ (status401, errorClientNotAuthenticated)
  , (status403, errorAccessForbidden)
  , (status400, errorBadRequest)
  , (status422, errorUnprocessableEntity)
  , (status409, errorConflict) ]

errorClientNotAuthenticated :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorClientNotAuthenticated s = ClientNotAuthenticated <$> decodeThrow s

errorConflict :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorConflict s = Conflict <$> decodeThrow s

errorUnprocessableEntity :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorUnprocessableEntity s = UnprocessableEntity <$> decodeThrow s

errorAccessForbidden :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorAccessForbidden s = AccessForbidden <$> decodeThrow s

errorTooManyRequests :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorTooManyRequests s = TooManyRequests <$> decodeThrow s

errorBadRequest :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBadRequest s = BadRequest <$> decodeThrow s

errorSubscriptionNotFound :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorSubscriptionNotFound s = SubscriptionNotFound <$> decodeThrow s

errorCursorAlreadyCommitted :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorCursorAlreadyCommitted s = CursorAlreadyCommitted <$> decodeThrow s

errorCursorResetInProgress :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorCursorResetInProgress s = CursorResetInProgress <$> decodeThrow s

errorEventTypeNotFound :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorEventTypeNotFound s = EventTypeNotFound <$> decodeThrow s

errorSubscriptionExistsAlready :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorSubscriptionExistsAlready s = SubscriptionExistsAlready <$> decodeThrow s

errorBatchPartiallySubmitted :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBatchPartiallySubmitted s = BatchPartiallySubmitted <$> decodeThrow s

errorBatchNotSubmitted :: MonadThrow m => ByteString.Lazy.ByteString -> m NakadiException
errorBatchNotSubmitted s = BatchNotSubmitted <$> decodeThrow s
