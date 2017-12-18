{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module AWSLambda.Events.Records where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson      (FromJSON (..), withObject, (.:))

newtype RecordsEvent a = RecordsEvent { _reRecords :: [a] } deriving (Eq, Show)

instance FromJSON a => FromJSON (RecordsEvent a) where
  parseJSON = withObject "RecordsEvent" $ \o -> RecordsEvent <$> o .: "Records"

$(makeLenses ''RecordsEvent)
