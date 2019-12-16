{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.SQSEvent
Description: Types for SQS Lambda events
-}
module AWSLambda.Events.SQSEvent where

import           Control.Lens
import           Data.Aeson               (FromJSON (..), genericParseJSON)
import           Data.Aeson.Casing        (aesonPrefix, camelCase)
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString          (ByteString)
import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Network.AWS.Data.Base64
import           Network.AWS.Data.Text    (FromText)
import qualified Network.AWS.Types        as AWS

import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records

data SQSMessage body = SQSMessage
  { _sqsmMessageId         :: !Text
  , _sqsmReceiptHandle     :: !Text
  , _sqsmBody              :: !(TextValue body)
  , _sqsmAttributes        :: !(HashMap Text Text)
  , _sqsmMessageAttributes :: !(HashMap Text MessageAttribute)
  , _sqsmMd5OfBody         :: !Text
  , _sqsmEventSource       :: !Text
  , _sqsmEventSourceARN    :: !Text
  , _sqsmAwsRegion         :: !AWS.Region
  } deriving (Show, Eq, Generic)

instance FromText message => FromJSON (SQSMessage message) where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

$(makeLenses ''SQSMessage)

type SQSEvent body = RecordsEvent (SQSMessage body)

-- | A Traversal to get messages from an SQSEvent
sqsMessages :: Traversal (SQSEvent message) (SQSEvent message') message message'
sqsMessages = reRecords . traverse . sqsmBody . unTextValue

-- | A Traversal to get embedded JSON values from an SQSEvent
sqsEmbedded :: Traversal (SQSEvent (Embedded v)) (SQSEvent (Embedded v')) v v'
sqsEmbedded = sqsMessages . unEmbed

sqsBinary :: Traversal' (SQSEvent Base64) ByteString
sqsBinary = sqsMessages . _Base64
