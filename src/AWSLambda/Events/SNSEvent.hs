{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: AWSLambda.Events.SNSEvent
Description: Types for SNS Lambda events

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.SNSEvents
-}
module AWSLambda.Events.SNSEvent where

import           Control.Applicative ((<|>))
import           Control.Exception.Safe (MonadCatch)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
                  (FromJSON(..), genericParseJSON, withObject, (.!=), (.:), (.:?))
import           Data.Aeson.Casing (aesonDrop, pascalCase)
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import           Amazonka.Data.Base64
import           Amazonka.Data.Text (FromText)

import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Handler (lambdaMain)

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

-- When a lambda is triggered directly off of an SNS topic,
-- the SNS message contains message attributes and the URI
-- fields are cased as `SigningCertUrl` and `UnsubscribeUrl`.
-- When an SNS message is embedded in an SQS event,
-- the SNS message changes in two ways; `MessageAttributes`
-- is not present and the casing for the URI fields becomes
-- `SigningCertURL` and `UnsubscribeURL`.
-- For these reasons we must hand-roll the `FromJSON` instance.
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

-- | Traverse an SNS message
traverseSnsMessage :: (FromJSON a, Applicative m) => (a -> m ()) -> SNSMessage (Embedded a) -> m ()
traverseSnsMessage act message =
    act $ message ^. smMessage . unTextValue . unEmbed

-- | Traverse all the messages in an SNS event
traverseSns :: (FromJSON a, Applicative m) => (a -> m ()) -> SNSEvent (Embedded a) -> m ()
traverseSns act = traverseRecords $ \record ->
    act $ record ^. srSns . smMessage . unTextValue . unEmbed

-- | A specialed version of the 'lambdaMain' entry-point
-- for handling individual SNS messages
snsMain :: (FromJSON a, MonadCatch m, MonadIO m) => (a -> m ()) -> m ()
snsMain = lambdaMain . traverseSns
