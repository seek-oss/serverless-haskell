{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
{-# LANGUAGE LambdaCase #-}

module AWSLambda.Handler
  ( lambdaMain
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson

import qualified Data.ByteString as ByteString

import Data.Maybe (isJust)

import qualified Data.Text.Lazy.IO as Text

import GHC.IO.Handle (Handle, hClose)

import Network

import System.Environment (lookupEnv)
import System.IO (hFlush, hPrint, stdin, stdout)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..))

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
  isLambda >>= \case
    True -> lambdaMain' act
    False -> localMain act

-- | Process the requests from the JavaScript wrapper using the provided action.
lambdaMain' ::
     (Aeson.FromJSON event, Aeson.ToJSON res)
  => (event -> IO res) -- ^ Function to process the event
  -> IO ()
lambdaMain' act = do
  listenSocket <- listenOn $ PortNumber 0
  -- Communicate the allocated port number to the JavaScript wrapper
  commHandle <- fdToHandle communicationFd
  PortNumber port <- socketPort listenSocket
  hPrint commHandle port
  hFlush commHandle
  forever $ do
    (socket, _, _) <- accept listenSocket
    forkIO $ finally (handleEvent socket socket act) (hFlush stdout >> hClose socket)

-- | Run the action outside the AWS Lambda, using the process' standard input
-- and standard output.
localMain ::
     (Aeson.FromJSON event, Aeson.ToJSON res)
  => (event -> IO res) -- ^ Function to process the event
  -> IO ()
localMain = handleEvent stdin stdout

-- | Process an event from the given input channel and write the result to the
-- given output channel.
handleEvent ::
     (Aeson.FromJSON event, Aeson.ToJSON res)
  => Handle -- ^ Input channel
  -> Handle -- ^ Output channel
  -> (event -> IO res) -- ^ Function to process the event
  -> IO ()
handleEvent inputChan outputChan act = do
  input <- ByteString.hGetLine inputChan
  case Aeson.eitherDecodeStrict input of
    Left err -> error err
    Right event -> do
      result <- act event
      Text.hPutStrLn outputChan $ Aeson.encodeToLazyText result

-- | Whether the code is running on AWS Lambda.
isLambda :: IO Bool
isLambda = isJust <$> lookupEnv "AWS_LAMBDA_FUNCTION_NAME"

-- | File descriptor opened by the JavaScript wrapper to communicate the process
-- state
communicationFd :: Fd
communicationFd = Fd 3
