{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.KinesisEvent
Description: Types for Kinesis Lambda events

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.KinesisEvents
-}
module AWSLambda.Events.KinesisEvent where

import           Control.Lens.TH
import           Data.Aeson                (FromJSON (..), withObject, (.:))
import           Data.Aeson.Casing         (aesonDrop, camelCase)
import           Data.Aeson.TH             (deriveFromJSON)
import           Data.Text                 (Text)
import           Amazonka.Data.Base64      (Base64 (..))
import qualified Amazonka.Kinesis.Types    as Kinesis
import qualified Amazonka.Types            as AWS

import           AWSLambda.Events.Records

data KinesisRecord = KinesisRecord
  { _krRecord               :: !Kinesis.Record
  , _krKinesisSchemaVersion :: !Text
  } deriving (Eq, Show)

instance FromJSON KinesisRecord where
  parseJSON =
    withObject "KinesisRecord" $
    \o -> do
      _krKinesisSchemaVersion <- o .: "kinesisSchemaVersion"
      dataBase64 <- o .: "data"
      _krRecord <-
        Kinesis.newRecord <$> (o .: "sequenceNumber") <*> pure (unBase64 dataBase64) <*>
        (o .: "partitionKey")
      return KinesisRecord {..}
$(makeLenses ''KinesisRecord)

data KinesisEventRecord = KinesisEventRecord
  { _kerKinesis           :: !KinesisRecord
  , _kerEventSource       :: !Text
  , _kerEventID           :: !Text
  , _kerInvokeIdentityArn :: !Text
  , _kerEventVersion      :: !Text
  , _kerEventName         :: !Text
  , _kerEventSourceARN    :: !Text
  , _kerAwsRegion         :: !AWS.Region
  } deriving (Eq, Show)
$(deriveFromJSON (aesonDrop 4 camelCase) ''KinesisEventRecord)
$(makeLenses ''KinesisEventRecord)

type KinesisEvent = RecordsEvent KinesisEventRecord
