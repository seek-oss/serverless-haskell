{-|
Main module for the integration test.
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import AWSLambda

import Control.Monad (when)

import System.Environment

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  -- Test logs going through
  putStrLn "This should go to logs"
  -- Test passed event
  print evt
  -- Throw error on a magic input value
  when (evt == errorEvent) $ error "Magic error"
  -- Test return value
  pure [11, 22, 33]

errorEvent :: Aeson.Value
errorEvent = Aeson.object ["error" .= (1 :: Int)]
