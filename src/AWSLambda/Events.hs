{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Events
  ( module AWSLambda.Events.APIGateway
  , module AWSLambda.Events.KinesisEvent
  , module AWSLambda.Events.MessageAttribute
  , module AWSLambda.Events.Records
  , module AWSLambda.Events.S3Event
  , module AWSLambda.Events.SNSEvent
  , module AWSLambda.Events.SQSEvent
  , snsInSqsMain
  , s3InSnsInSqsMain
  ) where

import           Control.Exception.Safe (MonadCatch)
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON(..))

import           AWSLambda.Events.APIGateway
import           AWSLambda.Events.KinesisEvent
import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent
import           AWSLambda.Events.SQSEvent

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual SNS messages embedded in SQS messages
snsInSqsMain :: (FromJSON a, MonadCatch m, MonadIO m) => (a -> m ()) -> m ()
snsInSqsMain = sqsMain . traverseSns

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual S3 event notifications embedded in
-- SNS messages embedded in SQS messages
s3InSnsInSqsMain :: (MonadCatch m, MonadIO m) => (S3EventNotification -> m ()) -> m ()
s3InSnsInSqsMain = snsInSqsMain . traverseRecords
