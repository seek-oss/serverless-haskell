{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: Data.Aeson.Embedded
Description: Type for a JSON value embedded within a JSON string value
-}
module Data.Aeson.Embedded where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Text.Encoding (encodeUtf8)

-- | Type for a JSON value embedded within a JSON string value
newtype Embedded a = Embedded { _unEmbed :: a } deriving (Eq, Show)

instance FromJSON a =>
         FromJSON (Embedded a) where
  parseJSON =
    withText "Embedded JSON" $
    fmap Embedded . either fail pure . eitherDecodeStrict . encodeUtf8

$(makeLenses ''Embedded)
