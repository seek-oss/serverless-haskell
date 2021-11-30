{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Module: Data.Aeson.TextValue
Description: Type for things that can be embedded in a JSON string

Provides @FromJSON@ and @ToJSON@ instances for anything that
has @FromText@ and @ToText@ instances, e.g. @TextValue Text@,
@(FromJSON a, ToJSON a) => TextValue (Embedded a)@,
@TextValue Base64@
-}
module Data.Aeson.TextValue where

import           Control.Lens.TH
import           Data.Aeson
import           Data.String
import           Amazonka.Data.Text (FromText (..), ToText (..), fromText)


newtype TextValue a = TextValue { _unTextValue :: a } deriving (Eq, Show, IsString)

instance FromText a => FromJSON (TextValue a) where
  parseJSON = withText "TextValue" $ fmap TextValue . either fail pure . fromText

instance ToText a => ToJSON (TextValue a) where
  toJSON = toJSON . toText . _unTextValue
  toEncoding = toEncoding . toText . _unTextValue

$(makeLenses ''TextValue)
