{-|
Main module for a subproject in the integration test.
-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import AWSLambda

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO [Text]
handler _ = do
  pure ["subdir result"]
