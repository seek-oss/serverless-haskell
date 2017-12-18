{-# LANGUAGE TemplateHaskell   #-}
module AWSLambda.Events where

import           Control.Applicative      ((<|>))
import           Control.Lens.TH
import           Data.Aeson               (FromJSON (..), Value)

import           AWSLambda.Events.S3Event

data DynamoDBEvent
data KinesisStreamEvent
data SNSEvent
data SESEvent
data CognitoEvent
data CloudFormationEvent
data CloudWatchLogsEvent
data CloudWatchEventsEvent
data CodeCommitEvent
data ConfigEvent
data AlexaEvent
data LexEvent
data APIGatewayEvent
data IoTButtonEvent
data CloudFrontEvent
data FirehoseEvent
data InvokeEvent

data LambdaEvent
  = S3 !S3Event
  | DynamoDB !DynamoDBEvent
  | KinesisStream !KinesisStreamEvent
  | SNS !SNSEvent
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

instance FromJSON LambdaEvent where
  parseJSON v = S3 <$> parseJSON v <|> pure (Custom v)
$(makePrisms ''LambdaEvent)
