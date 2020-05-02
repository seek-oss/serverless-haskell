{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
module AWSLambda.Handler
  ( lambdaMain
  ) where

import Control.Monad (forever)

import qualified Data.Aeson as Aeson

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import GHC.IO.Handle (BufferMode(..), hSetBuffering)

import Network.HTTP.Client (Request, RequestBody(..), Response, defaultManagerSettings, httpLbs, httpNoBody, method, newManager, parseRequest_, requestBody, responseBody, responseHeaders)
import Network.HTTP.Types (HeaderName)

import System.Environment (lookupEnv)
import System.IO (stdout)

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
     (Aeson.FromJSON event, Aeson.ToJSON res)
  => (event -> IO res) -- ^ Function to process the event
  -> IO ()
lambdaMain act =
  runMain $ \input -> do
    case Aeson.eitherDecode input of
      Left err -> error err
      Right event -> do
        result <- act event
        pure $ Aeson.encode result

-- Blah. Also set line buffering on standard output for AWS Lambda so the logs
-- are output in a timely manner.
runMain :: (LBS.ByteString -> IO LBS.ByteString) -> IO ()
runMain act = do
  lambdaApiAddress <- lookupEnv lambdaApiAddressEnv
  case lambdaApiAddress of
    Just address -> do
      hSetBuffering stdout LineBuffering
      manager <- newManager defaultManagerSettings
      forever $ do
        invocation <- httpLbs (invocationRequest address) manager
        let input = responseBody invocation
        let requestId = responseRequestId invocation
        result <- act input
        _ <- httpNoBody (resultRequest address requestId result) manager
        pure ()
    Nothing -> do
      input <- LBS.fromStrict <$> ByteString.getLine
      result <- act input
      Text.putStrLn $ Text.decodeUtf8 $ LBS.toStrict result

lambdaApiAddressEnv :: String
lambdaApiAddressEnv = "AWS_LAMBDA_RUNTIME_API"

lambdaRequest :: String -> String -> Request
lambdaRequest apiAddress path = parseRequest_ $ "http://" ++ apiAddress ++ "/2018-06-01" ++ path

invocationRequest :: String -> Request
invocationRequest apiAddress = lambdaRequest apiAddress "/runtime/invocation/next"

resultRequest :: String -> String -> LBS.ByteString -> Request
resultRequest apiAddress requestId result = (lambdaRequest apiAddress $ "/runtime/invocation/" ++ requestId ++ "/response") { method = "POST", requestBody = RequestBodyLBS result }

requestIdHeader :: HeaderName
requestIdHeader = "Lambda-Runtime-Aws-Request-Id"

responseRequestId :: Response a -> String
responseRequestId = Char8.unpack . snd . head . filter (uncurry $ \h _ -> h == requestIdHeader) . responseHeaders
