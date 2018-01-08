{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Events where

import           Control.Applicative           ((<|>))
import           Control.Lens.TH
import           Data.Aeson                    (FromJSON (..), Value)

import           AWSLambda.Events.KinesisEvent
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent

-- | Not yet implemented
data DynamoDBEvent

-- | Not yet implemented
data SESEvent

-- | Not yet implemented
data CognitoEvent

-- | Not yet implemented
data CloudFormationEvent

-- | Not yet implemented
data CloudWatchLogsEvent

-- | Not yet implemented
data CloudWatchEventsEvent

-- | Not yet implemented
data CodeCommitEvent

-- | Not yet implemented
data ConfigEvent

-- | Not yet implemented
data AlexaEvent

-- | Not yet implemented
data LexEvent

-- | Not yet implemented
data APIGatewayEvent

-- | Not yet implemented
data IoTButtonEvent

-- | Not yet implemented
data CloudFrontEvent

-- | Not yet implemented
data FirehoseEvent

-- | Not yet implemented
data InvokeEvent

-- | Sum type for all possible Lambda events.
-- Parameterised on the type of SNS Events to be handled.
-- See @SNSEvent@ for details.
data LambdaEvent snsMessage
  = S3 !S3Event
  | DynamoDB !DynamoDBEvent
  | KinesisStream !KinesisEvent
  | SNS !(SNSEvent snsMessage)
  | SES !SESEvent
  | Cognito !CognitoEvent
  | CloudFormation !CloudFormationEvent
  | CloudWatchLogs !CloudWatchLogsEvent
  | CloudWatchEvents !CloudWatchEventsEvent
  | CodeCommit !CodeCommitEvent
  | Config !ConfigEvent
  | Alexa !AlexaEvent
  | Lex !LexEvent
  | APIGateway !APIGatewayEvent
  | IoTButton !IoTButtonEvent
  | CloudFront !CloudFrontEvent
  | Firehose !FirehoseEvent
  | Invoke !InvokeEvent
  | Custom !Value

-- | Attempt to parse the various event types.
-- Any valid JSON that can't be parsed as a specific
-- event type will result in a 'Custom' value.
instance FromJSON snsMessage =>
         FromJSON (LambdaEvent snsMessage) where
  parseJSON v =
    try S3 v <|> try KinesisStream v <|> try SNS v <|> pure (Custom v)
    where
      try f = fmap f . parseJSON

$(makePrisms ''LambdaEvent)
