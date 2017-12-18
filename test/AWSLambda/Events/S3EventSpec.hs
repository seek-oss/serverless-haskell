{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AWSLambda.Events.S3EventSpec where

import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event

import           Data.Aeson
import           Data.Aeson.QQ
import           Data.ByteString           (ByteString)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.AWS.S3

import           Test.Hspec

spec :: Spec
spec =
  describe "S3Event" $ do
    it "parses sample Put event" $
      fromJSON sampleS3PutJSON `shouldBe` Success sampleS3PutEvent
    it "parses sample Delete event" $
      fromJSON sampleS3DeleteJSON `shouldBe` Success sampleS3DeleteEvent

sampleS3PutJSON :: Value
sampleS3PutJSON = [aesonQQ|
{
  "Records": [
    {
      "eventVersion": "2.0",
      "eventTime": "1970-01-01T00:00:00.000Z",
      "requestParameters": {
        "sourceIPAddress": "127.0.0.1"
      },
      "s3": {
        "configurationId": "testConfigRule",
        "object": {
          "eTag": "0123456789abcdef0123456789abcdef",
          "sequencer": "0A1B2C3D4E5F678901",
          "key": "HappyFace.jpg",
          "size": 1024
        },
        "bucket": {
          "arn": "bucketarn",
          "name": "sourcebucket",
          "ownerIdentity": {
            "principalId": "EXAMPLE"
          }
        },
        "s3SchemaVersion": "1.0"
      },
      "responseElements": {
        "x-amz-id-2": "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH",
        "x-amz-request-id": "EXAMPLE123456789"
      },
      "awsRegion": "us-east-1",
      "eventName": "ObjectCreated:Put",
      "userIdentity": {
        "principalId": "EXAMPLE"
      },
      "eventSource": "aws:s3"
    }
  ]
}
|]

sampleS3PutEvent :: S3Event
sampleS3PutEvent =
  RecordsEvent
  { _reRecords =
    [ S3EventNotification
      { _senAwsRegion = NorthVirginia
      , _senEventName = S3ObjectCreatedPut
      , _senEventSource = "aws:s3"
      , _senEventTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
      , _senEventVersion = "2.0"
      , _senRequestParameters =
        RequestParametersEntity
        { _rpeSourceIPAddress = "127.0.0.1"
        }
      , _senResponseElements =
        ResponseElementsEntity
        { _reeXAmzId2 =
          "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH"
        , _reeXAmzRequestId = "EXAMPLE123456789"
        }
      , _senS3 =
        S3Entity
        { _seBucket =
          S3BucketEntity
          { _sbeArn = "bucketarn"
          , _sbeName = BucketName "sourcebucket"
          , _sbeOwnerIdentity =
            UserIdentityEntity
            { _uiePrincipalId = "EXAMPLE"
            }
          }
        , _seConfigurationId = "testConfigRule"
        , _seObject =
          S3ObjectEntity
          { _soeETag = Just (ETag "0123456789abcdef0123456789abcdef")
          , _soeKey = ObjectKey "HappyFace.jpg"
          , _soeSize = Just 1024
          , _soeSequencer = "0A1B2C3D4E5F678901"
          , _soeVersionId = Nothing
          }
        , _seS3SchemaVersion = "1.0"
        }
      , _senUserIdentity =
        UserIdentityEntity
        { _uiePrincipalId = "EXAMPLE"
        }
      }
    ]
  }

sampleS3DeleteJSON :: Value
sampleS3DeleteJSON = [aesonQQ|
  {
  "Records": [
    {
      "eventVersion": "2.0",
      "eventTime": "1970-01-01T00:00:00.000Z",
      "requestParameters": {
        "sourceIPAddress": "127.0.0.1"
      },
      "s3": {
        "configurationId": "testConfigRule",
        "object": {
          "sequencer": "0A1B2C3D4E5F678901",
          "key": "HappyFace.jpg"
        },
        "bucket": {
          "arn": "bucketarn",
          "name": "sourcebucket",
          "ownerIdentity": {
            "principalId": "EXAMPLE"
          }
        },
        "s3SchemaVersion": "1.0"
      },
      "responseElements": {
        "x-amz-id-2": "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH",
        "x-amz-request-id": "EXAMPLE123456789"
      },
      "awsRegion": "us-east-1",
      "eventName": "ObjectRemoved:Delete",
      "userIdentity": {
        "principalId": "EXAMPLE"
      },
      "eventSource": "aws:s3"
    }
  ]
}
|]

sampleS3DeleteEvent :: S3Event
sampleS3DeleteEvent =
  RecordsEvent
  { _reRecords =
    [ S3EventNotification
      { _senAwsRegion = NorthVirginia
      , _senEventName = S3ObjectRemovedDelete
      , _senEventSource = "aws:s3"
      , _senEventTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
      , _senEventVersion = "2.0"
      , _senRequestParameters =
        RequestParametersEntity
        { _rpeSourceIPAddress = "127.0.0.1"
        }
      , _senResponseElements =
        ResponseElementsEntity
        { _reeXAmzId2 =
          "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH"
        , _reeXAmzRequestId = "EXAMPLE123456789"
        }
      , _senS3 =
        S3Entity
        { _seBucket =
          S3BucketEntity
          { _sbeArn = "bucketarn"
          , _sbeName = BucketName "sourcebucket"
          , _sbeOwnerIdentity =
            UserIdentityEntity
            { _uiePrincipalId = "EXAMPLE"
            }
          }
        , _seConfigurationId = "testConfigRule"
        , _seObject =
          S3ObjectEntity
          { _soeETag = Nothing
          , _soeKey = ObjectKey "HappyFace.jpg"
          , _soeSize = Nothing
          , _soeSequencer = "0A1B2C3D4E5F678901"
          , _soeVersionId = Nothing
          }
        , _seS3SchemaVersion = "1.0"
        }
      , _senUserIdentity =
        UserIdentityEntity
        { _uiePrincipalId = "EXAMPLE"
        }
      }
    ]
  }
