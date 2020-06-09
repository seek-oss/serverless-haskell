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
  , traverseSnsInSqs
  , snsInSqsMain
  , traverseS3InSnsInSqs
  , s3InSnsInSqsMain
  ) where

import           Control.Exception.Safe            (MonadCatch)
import           Control.Monad.IO.Class
import           Data.Aeson                        (FromJSON (..))

import           AWSLambda.Events.APIGateway
import           AWSLambda.Events.KinesisEvent
import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent
import           AWSLambda.Events.SQSEvent
import           AWSLambda.Handler                 (lambdaMain)
import           Data.Aeson.Embedded               (Embedded)

-- | Traverse all the SNS messages embedded in an SQS event
traverseSnsInSqs :: (FromJSON a, Applicative m) => (a -> m ()) -> SQSEvent (Embedded (SNSMessage (Embedded a))) -> m ()
traverseSnsInSqs = traverseSqs . traverseSnsMessage

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual SNS messages embedded in an SQS event
snsInSqsMain :: (FromJSON a, MonadCatch m, MonadIO m) => (a -> m ()) -> m ()
snsInSqsMain = lambdaMain . traverseSnsInSqs

-- | Traverse S3 events embedded within SNS messages within an SQS event
traverseS3InSnsInSqs :: (Applicative m) => (S3EventNotification -> m ()) -> SQSEvent (Embedded (SNSMessage (Embedded S3Event))) -> m ()
traverseS3InSnsInSqs = traverseSnsInSqs . traverseRecords

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual S3 event notifications embedded in
-- SNS messages embedded in an SQS event
s3InSnsInSqsMain :: (MonadCatch m, MonadIO m) => (S3EventNotification -> m ()) -> m ()
s3InSnsInSqsMain = lambdaMain . traverseS3InSnsInSqs
