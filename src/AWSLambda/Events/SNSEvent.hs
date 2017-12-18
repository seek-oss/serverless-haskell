{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module AWSLambda.Events.SNSEvent where

-- | Types for SNS Lambda events.
-- Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.SNSEvents

import           Control.Lens.TH
import           Data.Aeson.Casing        (aesonDrop, pascalCase)
import           Data.Aeson.TH            (deriveFromJSON)
import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)

import           AWSLambda.Events.Records

data MessageAttribute = MessageAttribute
  { _maType :: !Text
  , _maValue :: !Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 pascalCase) ''MessageAttribute)
$(makeLenses ''MessageAttribute)

data SNSMessage = SNSMessage
  { _smMessage           :: !Text
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
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 pascalCase) ''SNSMessage)
$(makeLenses ''SNSMessage)

data SNSRecord = SNSRecord
  { _srEventVersion         :: !Text
  , _srEventSubscriptionArn :: !Text
  , _srEventSource          :: !Text
  , _srSns                  :: !SNSMessage
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 pascalCase) ''SNSRecord)
$(makeLenses ''SNSRecord)

type SNSEvent = RecordsEvent SNSRecord
