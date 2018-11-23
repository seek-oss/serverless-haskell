{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
module AWSLambda.Handler
  ( lambdaMain
  ) where

import Control.Exception (finally)
import Control.Monad (void)

import qualified Data.Aeson as Aeson

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import GHC.IO.Handle (BufferMode(..), hSetBuffering)

import Network.Simple.TCP (connect)
import Network.Socket (close, withSocketsDo)
import Network.Socket.ByteString (send)

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
  withSendResult $ \sendResult -> do
    input <- ByteString.getLine
    case Aeson.eitherDecodeStrict input of
      Left err -> error err
      Right event -> do
        result <- act event
        sendResult $ LBS.toStrict $ Aeson.encode result
        pure ()

-- | Invoke an action with the method to output the result. If called by the
-- JavaScript wrapper, use the server started by it, otherwise use standard
-- output. Also set line buffering on standard output for AWS Lambda so the logs
-- are output in a timely manner.
withSendResult :: ((ByteString -> IO ()) -> IO r) -> IO r
withSendResult act = do
  communicationPortString <- lookupEnv communicationPortEnv
  case communicationPortString of
    Just communicationPort -> do
      hSetBuffering stdout LineBuffering
      withSocketsDo $
        connect "127.0.0.1" communicationPort $ \(socket, _) ->
          act (void . send socket) `finally` close socket
    Nothing -> act $ Text.putStrLn . Text.decodeUtf8

-- | Environment variable signalling the port the JavaScript wrapper is
-- listening on
communicationPortEnv :: String
communicationPortEnv = "SERVERLESS_HASKELL_COMMUNICATION_PORT"
