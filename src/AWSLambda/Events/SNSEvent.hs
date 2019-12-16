{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.SNSEvent
Description: Types for SNS Lambda events

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.SNSEvents
-}
module AWSLambda.Events.SNSEvent where

import           Control.Applicative      ((<|>))
import           Control.Lens
import           Data.Aeson               (FromJSON (..), genericParseJSON,
                                           withObject, (.:), (.:?), (.!=))
import           Data.Aeson.Casing        (aesonDrop, pascalCase)
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString          (ByteString)
import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           GHC.Generics             (Generic)
import           Network.AWS.Data.Base64
import           Network.AWS.Data.Text    (FromText)

import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records

data SNSMessage message = SNSMessage
  { _smMessage           :: !(TextValue message )
  , _smMessageAttributes :: !(HashMap Text MessageAttribute)
  , _smMessageId         :: !Text
  , _smSignature         :: !Text
  , _smSignatureVersion  :: !Text
  , _smSigningCertUrl    :: !Text
  , _smSubject           :: !Text
  , _smTimestamp         :: !UTCTime
  , _smTopicArn          :: !Text
  , _smType              :: !Text
  , _smUnsubscribeUrl    :: !Text
  } deriving (Eq, Show, Generic)

instance FromText message => FromJSON (SNSMessage message) where
  parseJSON = withObject "SNSMessage'" $ \o ->
    SNSMessage
      <$> o .: "Message"
      <*> o .:? "MessageAttributes" .!= mempty
      <*> o .: "MessageId"
      <*> o .: "Signature"
      <*> o .: "SignatureVersion"
      <*> do o .: "SigningCertUrl" <|> o .: "SigningCertURL"
      <*> o .: "Subject"
      <*> o .: "Timestamp"
      <*> o .: "TopicArn"
      <*> o .: "Type"
      <*> do o .: "UnsubscribeUrl" <|> o .: "UnsubscribeURL"

$(makeLenses ''SNSMessage)

data SNSRecord message = SNSRecord
  { _srEventVersion         :: !Text
  , _srEventSubscriptionArn :: !Text
  , _srEventSource          :: !Text
  , _srSns                  :: !(SNSMessage message)
  } deriving (Eq, Show, Generic)

instance FromText message => FromJSON (SNSRecord message) where
  parseJSON = genericParseJSON $ aesonDrop 3 pascalCase

$(makeLenses ''SNSRecord)

-- | SNSEvent.
-- The 'message' type is parameterised. To treat it as a text value
-- use @SNSEvent Text@.
-- To extract an embedded event object use the 'Embedded' type.
-- E.g. @SNSEvent (Embedded S3Event)@ will treat the message
-- as an embedded S3Event.
-- To extract embedded Base64 encoded binary use
-- @SNSEvent Base64@
type SNSEvent message = RecordsEvent (SNSRecord message)

-- | A Traversal to get messages from an SNSEvent
messages :: Traversal (SNSEvent message) (SNSEvent message') message message'
messages = reRecords . traverse . srSns . smMessage . unTextValue

-- | A Traversal to get embedded JSON values from an SNSEvent
embedded :: Traversal (SNSEvent (Embedded v)) (SNSEvent (Embedded v')) v v'
embedded = messages . unEmbed

binary :: Traversal' (SNSEvent Base64) ByteString
binary = messages . _Base64
