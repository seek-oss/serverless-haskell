{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.Embedded
Description: Type for JSON object embedded within a JSON string value
-}
module AWSLambda.Events.Embedded where

import           Control.Lens.TH
import           Data.Aeson
import Data.Text.Encoding        (encodeUtf8)

newtype Embedded a = Embedded { _unEmbed :: a } deriving (Eq, Show)

instance FromJSON a =>
         FromJSON (Embedded a) where
  parseJSON =
    withText "Embedded JSON" $
    fmap Embedded . either fail pure . eitherDecodeStrict . encodeUtf8

$(makeLenses ''Embedded)
