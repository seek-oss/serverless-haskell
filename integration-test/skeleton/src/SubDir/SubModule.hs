{-# LANGUAGE OverloadedStrings #-}
module SubDir.SubModule where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import AWSLambda

handler :: Aeson.Value -> Context -> IO (Either Text [Text])
handler _ _ = do
  pure $ Right ["subdir result"]
