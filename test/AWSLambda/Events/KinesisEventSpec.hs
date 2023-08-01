{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AWSLambda.Events.KinesisEventSpec where

import           AWSLambda.Events.KinesisEvent
import           AWSLambda.Events.Records

import           Data.Aeson
import           Data.ByteString.Lazy          (ByteString)

import qualified Amazonka.Kinesis.Types        as Kinesis
import           Amazonka.Types                (Region (..))

import           Text.RawString.QQ

import           Test.Hspec

spec :: Spec
spec =
  describe "KinesisEvent" $
    it "parses sample event" $
      decode sampleKinesisJSON `shouldBe` Just sampleKinesisEvent

sampleKinesisJSON :: ByteString
sampleKinesisJSON = [r|
{
    "Records": [
        {
            "kinesis": {
                "partitionKey": "partitionKey-3",
                "kinesisSchemaVersion": "1.0",
                "data": "SGVsbG8sIHRoaXMgaXMgYSB0ZXN0IDEyMy4=",
                "sequenceNumber": "49545115243490985018280067714973144582180062593244200961"
            },
            "eventSource": "aws:kinesis",
            "eventID": "shardId-000000000000:49545115243490985018280067714973144582180062593244200961",
            "invokeIdentityArn": "arn:aws:iam::account-id:role/testLEBRole",
            "eventVersion": "1.0",
            "eventName": "aws:kinesis:record",
            "eventSourceARN": "arn:aws:kinesis:us-west-2:35667example:stream/examplestream",
            "awsRegion": "us-west-2"
        }
    ]
}
|]

sampleKinesisEvent :: KinesisEvent
sampleKinesisEvent =
  RecordsEvent
    [ KinesisEventRecord
      { _kerKinesis =
        KinesisRecord
        { _krRecord =
          Kinesis.newRecord
            "49545115243490985018280067714973144582180062593244200961"
            "Hello, this is a test 123."
            "partitionKey-3"
        , _krKinesisSchemaVersion = "1.0"
        }
      , _kerEventSource = "aws:kinesis"
      , _kerEventID =
        "shardId-000000000000:49545115243490985018280067714973144582180062593244200961"
      , _kerInvokeIdentityArn = "arn:aws:iam::account-id:role/testLEBRole"
      , _kerEventVersion = "1.0"
      , _kerEventName = "aws:kinesis:record"
      , _kerEventSourceARN =
        "arn:aws:kinesis:us-west-2:35667example:stream/examplestream"
      , _kerAwsRegion = Oregon
      }
    ]
