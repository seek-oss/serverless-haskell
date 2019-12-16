{-# LANGUAGE TemplateHaskell #-}

{-|
Module: AWSLambda.Events.MessageAttribute
Description: Types for SQS and SNS message attributes
-}
module AWSLambda.Events.MessageAttribute where

import           Control.Lens             (makeLenses)
import           Data.Aeson.Casing        (aesonPrefix, pascalCase)
import           Data.Aeson.TH            (deriveFromJSON)
import           Data.Text                (Text)

data MessageAttribute = MessageAttribute
  { _maType  :: !Text
  , _maValue :: !Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonPrefix pascalCase) ''MessageAttribute)
$(makeLenses ''MessageAttribute)
