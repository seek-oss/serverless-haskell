{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
module AWSLambda.Handler
  ( lambdaMain
  , runMain
  ) where

import           Control.Exception.Safe (MonadCatch, SomeException(..), displayException, tryAny)
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import           Data.Typeable (typeOf)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import           GHC.IO.Handle (BufferMode(..), hSetBuffering)

import           Network.HTTP.Client
import           Network.HTTP.Types (HeaderName)

import           System.Environment (lookupEnv)
import           System.IO (stdout)

-- | Process incoming events from @serverless-haskell@ using a provided
-- function.
--
-- The handler receives the input event given to the AWS Lambda function, and
-- its return value is returned from the function.
--
-- This is intended to be used as @main@, for example:
--
-- > import qualified Data.Aeson as Aeson
-- >
-- > import AWSLambda
-- >
-- > main = lambdaMain handler
-- >
-- > handler :: Aeson.Value -> IO [Int]
-- > handler evt = do
-- >   putStrLn "This should go to logs"
-- >   print evt
-- >   pure [1, 2, 3]
--
-- The handler function can receive arbitrary JSON values from custom
-- invocations, or one of the events from the "AWSLambda.Events" module, such as
-- 'AWSLambda.Events.S3Event':
--
-- > import AWSLambda.Events.S3Event
-- >
-- > handler :: S3Event -> IO ()
-- > handler evt = do
-- >   print $ records evt
--
-- If the Lambda function needs to process several types of events, use
-- 'Data.Aeson.Alternative' to combine several handlers:
--
-- > import AWSLambda
-- > import AWSLambda.Events.S3Event
-- > import Data.Aeson
-- > import Data.Aeson.Alternative
-- >
-- > main = lambdaMain $ handlerS3 `alternative` handlerCustom
-- >
-- > handlerS3 :: S3Event -> IO ()
-- > handlerS3 = _
-- >
-- > handlerCustom :: Value -> IO ()
-- > handlerCustom = _
--
-- When run outside the AWS Lambda environment, the input is read as JSON from
-- the command line, and the result of the execution is printed, also as JSON,
-- to the standard output.
lambdaMain ::
     (Aeson.FromJSON event, Aeson.ToJSON res, MonadCatch m, MonadIO m)
  => (event -> m res) -- ^ Function to process the event
  -> m ()
lambdaMain act =
  runMain $ \input -> do
    case Aeson.eitherDecode input of
      Left err -> error err
      Right event -> do
        result <- act event
        pure $ Aeson.encode result

-- Process the incoming requests (using the AWS Lambda runtime interface or from the standard input).
-- Also set line buffering on standard output for AWS Lambda so the logs are output in a timely manner.
runMain :: (MonadCatch m, MonadIO m) => (LBS.ByteString -> m LBS.ByteString) -> m ()
runMain act = do
  lambdaApiAddress <- liftIO $ lookupEnv lambdaApiAddressEnv
  case lambdaApiAddress of
    Just address -> do
      liftIO $ hSetBuffering stdout LineBuffering
      manager <- liftIO $ newManager defaultManagerSettings
      forever $ do
        invocation <- liftIO $ httpLbs (invocationRequest address) manager
        let input = responseBody invocation
        let requestId = responseRequestId invocation
        resultOrError <- tryAny $ act input
        case resultOrError of
          Right result   -> liftIO $ void $ httpNoBody (resultRequest address requestId result) manager
          Left exception -> liftIO $ void $ httpNoBody (errorRequest address requestId exception) manager
    Nothing -> do
      input <- liftIO $ LBS.fromStrict <$> ByteString.getLine
      result <- act input
      liftIO $ Text.putStrLn $ Text.decodeUtf8 $ LBS.toStrict result

lambdaApiAddressEnv :: String
lambdaApiAddressEnv = "AWS_LAMBDA_RUNTIME_API"

lambdaRequest :: String -> String -> Request
lambdaRequest apiAddress rqPath = parseRequest_ $ "http://" ++ apiAddress ++ "/2018-06-01" ++ rqPath

invocationRequest :: String -> Request
invocationRequest apiAddress = (lambdaRequest apiAddress "/runtime/invocation/next") { responseTimeout = responseTimeoutNone }

resultRequest :: String -> String -> LBS.ByteString -> Request
resultRequest apiAddress requestId result = (lambdaRequest apiAddress $ "/runtime/invocation/" ++ requestId ++ "/response") { method = "POST", requestBody = RequestBodyLBS result }

errorRequest :: String -> String -> SomeException -> Request
errorRequest apiAddress requestId exception = (lambdaRequest apiAddress $ "/runtime/invocation/" ++ requestId ++ "/error") { method = "POST", requestBody = RequestBodyLBS body }
  where
    body = Aeson.encode $ Aeson.object [ "errorMessage" .= displayException exception, "errorType" .= exceptionType exception]

exceptionType :: SomeException -> String
exceptionType (SomeException e) = show (typeOf e)

requestIdHeader :: HeaderName
requestIdHeader = "Lambda-Runtime-Aws-Request-Id"

responseRequestId :: Response a -> String
responseRequestId = Char8.unpack . snd . head . filter (uncurry $ \h _ -> h == requestIdHeader) . responseHeaders
