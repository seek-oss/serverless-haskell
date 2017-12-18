{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Entry point for AWS Lambda handlers deployed with @serverless-haskell@ plugin.
-}
module AWSLambda.Handler
  ( lambdaMain
  ) where

import Control.Exception (bracket)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson

import qualified Data.ByteString as ByteString

import qualified Data.Text.Lazy.IO as Text

import GHC.IO.Handle (Handle, hClose)

import System.Environment (lookupEnv)
import System.IO (stdout)
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

-- | Invoke an action with the handle to write the results to. On AWS Lambda,
-- use the channel opened by the JavaScript wrapper, otherwise use standard
-- output.
withResultChannel :: (Handle -> IO r) -> IO r
withResultChannel act = do
  lambdaFunctionName <- lookupEnv "AWS_LAMBDA_FUNCTION_NAME"
  case lambdaFunctionName of
    Just _ -> bracket (fdToHandle communicationFd) hClose act
    Nothing -> act stdout

-- | File descriptor opened by the JavaScript wrapper to listen for the results
communicationFd :: Fd
communicationFd = Fd 3
