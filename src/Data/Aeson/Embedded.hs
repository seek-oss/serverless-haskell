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
import           Data.Text.Lazy            (Text)
import           Data.Text.Lazy.Encoding   (decodeUtf8, encodeUtf8)

-- | Type for a JSON value embedded within a JSON string value
newtype Embedded a = Embedded { _unEmbed :: a } deriving (Eq, Show)

instance FromJSON a =>
         FromJSON (Embedded a) where
  parseJSON v =
    fmap Embedded . either fail pure . eitherDecode . encodeUtf8 =<< parseJSON v

instance ToJSON a => ToJSON (Embedded a) where
  toJSON = toJSON . embed
  toEncoding = toEncoding . embed

embed :: ToJSON a => Embedded a -> Text
embed = decodeUtf8 . encode . _unEmbed

$(makeLenses ''Embedded)
