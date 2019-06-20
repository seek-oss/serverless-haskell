{-|
Main module for the integration test.
-}
{-# LANGUAGE OverloadedStrings #-}
module ThirdParty where

import qualified Data.Aeson as Aeson

import AWSLambda

import System.Environment

import Text.Regex.PCRE.Light

handler :: Aeson.Value -> Context -> IO (Either String [Int])
handler evt _ = do
  -- Test logs going through
  putStrLn "This should go to logs"
  -- Test passed arguments
  getArgs >>= print
  -- Test working with an included extra library (libpcre)
  print $ match (compile "[a-z]+" []) "012abc345" []
  -- Test passed event
  print evt
  -- Test return value
  pure $ Right [11, 22, 33]
