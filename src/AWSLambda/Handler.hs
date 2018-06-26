{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
{-# LANGUAGE TypeApplications #-}

module AWSLambda.Handler
  ( lambdaMain
  ) where

import Control.Exception (bracket, try)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson

import qualified Data.ByteString as ByteString

import qualified Data.Text.Lazy.IO as Text

import GHC.IO.Handle
       (BufferMode(..), Handle, hClose, hSetBuffering)

import System.IO (stdout)
import System.Posix.Files (getFdStatus)
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
  withResultChannel $ \resultChannel -> do
    input <- ByteString.getLine
    case Aeson.eitherDecodeStrict input of
      Left err -> error err
      Right event -> do
        result <- act event
        Text.hPutStrLn resultChannel $ Aeson.encodeToLazyText result
        pure ()

-- | Invoke an action with the handle to write the results to. If called by the
-- JavaScript wrapper, use the channel opened by it, otherwise use standard
-- output. Also set line buffering on standard output for AWS Lambda so the logs
-- are output in a timely manner.
withResultChannel :: (Handle -> IO r) -> IO r
withResultChannel act = do
  commStatus <- try @IOError $ getFdStatus communicationFd
  case commStatus of
    Right _ -> do
      hSetBuffering stdout LineBuffering
      bracket (fdToHandle communicationFd) hClose act
    Left _ -> act stdout

-- | File descriptor opened by the JavaScript wrapper to listen for the results
communicationFd :: Fd
communicationFd = Fd 3
