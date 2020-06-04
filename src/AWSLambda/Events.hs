{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Events
  ( module AWSLambda.Events.APIGateway
  , module AWSLambda.Events.KinesisEvent
  , module AWSLambda.Events.MessageAttribute
  , module AWSLambda.Events.Records
  , module AWSLambda.Events.S3Event
  , module AWSLambda.Events.SNSEvent
  , module AWSLambda.Events.SQSEvent
  ) where

import           AWSLambda.Events.APIGateway
import           AWSLambda.Events.KinesisEvent
import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent
import           AWSLambda.Events.SQSEvent
