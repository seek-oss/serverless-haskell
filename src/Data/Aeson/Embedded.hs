{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Module: Data.Aeson.Embedded
Description: Type for a JSON value embedded within a JSON string value
-}
module Data.Aeson.Embedded where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Extra      (encodeStrict)
import           Data.String
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Network.AWS.Data.Text (FromText (..), ToText (..), fromText,
                                        takeText)

efp :: Monad m => Either String a -> m a
efp = either fail pure

-- | Type for a JSON value embedded within a JSON string value
newtype Embedded a = Embedded { _unEmbed :: a } deriving (Eq, Show)

instance FromJSON a =>
         FromText (Embedded a) where
  parser =
    fmap Embedded . efp . eitherDecodeStrict . encodeUtf8 =<< takeText

instance FromJSON a =>
         FromJSON (Embedded a) where
  parseJSON v = efp . fromText =<< parseJSON v

instance ToJSON a => ToText (Embedded a) where
  toText = decodeUtf8 . encodeStrict . _unEmbed

instance ToJSON a => ToJSON (Embedded a) where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

$(makeLenses ''Embedded)

newtype TextValue a = TextValue { _unTextValue :: a } deriving (Eq, Show, IsString)

instance FromText a => FromJSON (TextValue a) where
  parseJSON = withText "TextValue" $ fmap TextValue . efp . fromText

instance ToText a => ToJSON (TextValue a) where
  toJSON = toJSON . toText . _unTextValue
  toEncoding = toEncoding . toText . _unTextValue

$(makeLenses ''TextValue)
