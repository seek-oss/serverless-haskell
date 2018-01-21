{-|
Main module for the integration test.
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson

import AWSLambda

import System.Environment

import Text.Regex.PCRE.Light

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  -- Test logs going through
  putStrLn "This should go to logs"
  -- Test passed arguments
  getArgs >>= print
  -- Test working with an included extra library (libpcre)
  print $ match (compile "[a-z]+" []) "012abc345" []
  -- Test passed event
  print evt
  -- Test return value
  pure [1, 2, 3]
