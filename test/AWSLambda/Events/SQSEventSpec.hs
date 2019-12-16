{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AWSLambda.Events.SQSEventSpec where

import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent
import           AWSLambda.Events.SQSEvent

import           Data.Aeson
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.AWS.S3            as S3

import           Text.RawString.QQ

import           Test.Hspec

spec :: Spec
spec = describe "Handler" $ do
  it "parses sample text event" $
    decode sampleSQSJSON `shouldBe` Just sampleSQSEvent
  it "parses sample embedded S3 -> SNS -> SQS event" $
    eitherDecode sampleS3SNSSQSJSON `shouldBe` Right sampleS3SNSSQSEvent

sampleSQSJSON :: ByteString
sampleSQSJSON = [r|
{
  "Records": [
    {
      "messageId": "b792b6ba-b444-48c5-9cd8-29b8ad373eae",
      "receiptHandle": "ReceiptHandle",
      "body": "Hello from SQS!",
      "attributes": {
        "ApproximateReceiveCount": "1",
        "SentTimestamp": "1572575717896",
        "SenderId": "AIDAIY4XCTD3OFZN5ED42",
        "ApproximateFirstReceiveTimestamp": "1572575717898"
      },
      "messageAttributes": {
        "Test": {
          "Type": "String",
          "Value": "TestString"
        },
        "TestBinary": {
          "Type": "Binary",
          "Value": "TestBinary"
        }
      },
      "md5OfBody": "aca137746cb7d6a8e3eda3d1ce09b0c5",
      "eventSource": "aws:sqs",
      "eventSourceARN": "arn:aws:sqs:ap-southeast-2:1234567890:my-queue",
      "awsRegion": "ap-southeast-2"
    }
  ]
}
|]

sampleSQSEvent :: SQSEvent Text
sampleSQSEvent =
  RecordsEvent
    [ SQSMessage
        { _sqsmMessageId         = "b792b6ba-b444-48c5-9cd8-29b8ad373eae"
        , _sqsmReceiptHandle     = "ReceiptHandle"
        , _sqsmBody              = "Hello from SQS!"
        , _sqsmAttributes        =
          [ ("ApproximateReceiveCount", "1")
          , ("SentTimestamp", "1572575717896")
          , ("SenderId", "AIDAIY4XCTD3OFZN5ED42")
          , ("ApproximateFirstReceiveTimestamp", "1572575717898")
          ]
        , _sqsmMessageAttributes =
          [ ( "Test"
            , MessageAttribute
              { _maType = "String"
              , _maValue = "TestString"
              })
          , ( "TestBinary"
            , MessageAttribute
              { _maType = "Binary"
              , _maValue = "TestBinary"
              })
          ]
        , _sqsmMd5OfBody         = "aca137746cb7d6a8e3eda3d1ce09b0c5"
        , _sqsmEventSource       = "aws:sqs"
        , _sqsmEventSourceARN    = "arn:aws:sqs:ap-southeast-2:1234567890:my-queue"
        , _sqsmAwsRegion         = Sydney
        }
      ]

sampleS3SNSSQSJSON :: ByteString
sampleS3SNSSQSJSON = [r|
{
  "Records": [
    {
      "messageId": "b792b6ba-b444-48c5-9cd8-29b8ad373eae",
      "receiptHandle": "ReceiptHandle",
      "body": "{\n  \"Type\" : \"Notification\",\n  \"MessageId\" : \"MessageId\",\n  \"TopicArn\" : \"arn:aws:sns:ap-southeast-2:11111111111111:my-topic\",\n  \"Subject\" : \"Amazon S3 Notification\",\n  \"Message\" : \"{\\\"Records\\\":[{\\\"eventVersion\\\":\\\"2.1\\\",\\\"eventSource\\\":\\\"aws:s3\\\",\\\"awsRegion\\\":\\\"ap-southeast-2\\\",\\\"eventTime\\\":\\\"2019-11-01T00:00:00.00Z\\\",\\\"eventName\\\":\\\"ObjectCreated:Put\\\",\\\"userIdentity\\\":{\\\"principalId\\\":\\\"AWS:AHJD568HF4356HJJ:bob\\\"},\\\"requestParameters\\\":{\\\"sourceIPAddress\\\":\\\"787.39.11.220\\\"},\\\"responseElements\\\":{\\\"x-amz-request-id\\\":\\\"GDJS6765sJSHSS\\\",\\\"x-amz-id-2\\\":\\\"ID2\\\"},\\\"s3\\\":{\\\"s3SchemaVersion\\\":\\\"1.0\\\",\\\"configurationId\\\":\\\"ConfigurationId\\\",\\\"bucket\\\":{\\\"name\\\":\\\"my-bucket\\\",\\\"ownerIdentity\\\":{\\\"principalId\\\":\\\"ASKD794UDYDH\\\"},\\\"arn\\\":\\\"arn:aws:s3:::my-bucket\\\"},\\\"object\\\":{\\\"key\\\":\\\"my-key\\\",\\\"size\\\":13315,\\\"eTag\\\":\\\"1231234fabf233124124\\\",\\\"versionId\\\":\\\"735hjf893ufb8fhuf\\\",\\\"sequencer\\\":\\\"HUJKFDHJD8656567HGGSGJKD\\\"}}}]}\",\n  \"Timestamp\" : \"2019-11-01T00:00:00Z\",\n  \"SignatureVersion\" : \"1\",\n  \"Signature\" : \"Signature\",\n  \"SigningCertURL\" : \"https://sns.ap-southeast-2.amazonaws.com/SimpleNotificationService-my-cert.pem\",\n  \"UnsubscribeURL\" : \"https://sns.ap-southeast-2.amazonaws.com/?Action=Unsubscribe&SubscriptionArn=arn:aws:sns:ap-southeast-2:11111111111111:my-topic:unsub\"\n}",
      "attributes": {
        "ApproximateReceiveCount": "1",
        "SentTimestamp": "1572575717896",
        "SenderId": "AIDAIY4XCTD3OFZN5ED42",
        "ApproximateFirstReceiveTimestamp": "1572575717898"
      },
      "messageAttributes": {
        "Test": {
          "Type": "String",
          "Value": "TestString"
        },
        "TestBinary": {
          "Type": "Binary",
          "Value": "TestBinary"
        }
      },
      "md5OfBody": "aca137746cb7d6a8e3eda3d1ce09b0c5",
      "eventSource": "aws:sqs",
      "eventSourceARN": "arn:aws:sqs:ap-southeast-2:1234567890:my-queue",
      "awsRegion": "ap-southeast-2"
    }
  ]
}
|]

sampleS3SNSSQSEvent :: SQSEvent (Embedded (SNSMessage (Embedded S3Event)))
sampleS3SNSSQSEvent =
  RecordsEvent
    [ SQSMessage
        { _sqsmMessageId         = "b792b6ba-b444-48c5-9cd8-29b8ad373eae"
        , _sqsmReceiptHandle     = "ReceiptHandle"
        , _sqsmBody              =
          TextValue $ Embedded $ SNSMessage
            { _smMessage                = TextValue $ Embedded $ RecordsEvent
              [ S3EventNotification
                { _senAwsRegion         = Sydney
                , _senEventName         = S3ObjectCreatedPut
                , _senEventSource       = "aws:s3"
                , _senEventTime         = UTCTime (fromGregorian 2019 11 1) 0
                , _senEventVersion      = "2.1"
                , _senRequestParameters = RequestParametersEntity "787.39.11.220"
                , _senResponseElements  = ResponseElementsEntity
                  { _reeXAmzId2         = "ID2"
                  , _reeXAmzRequestId   = "GDJS6765sJSHSS" }
                , _senS3                = S3Entity
                  { _seBucket           = S3BucketEntity
                    { _sbeArn           = "arn:aws:s3:::my-bucket"
                    , _sbeName          = BucketName "my-bucket"
                    , _sbeOwnerIdentity = UserIdentityEntity "ASKD794UDYDH" }
                  , _seConfigurationId  = "ConfigurationId"
                  , _seObject           = S3ObjectEntity
                    { _soeETag          = Just (ETag "1231234fabf233124124")
                    , _soeKey           = ObjectKey "my-key"
                    , _soeSize          = Just 13315
                    , _soeSequencer     = "HUJKFDHJD8656567HGGSGJKD"
                    , _soeVersionId     = Just "735hjf893ufb8fhuf" }
                  , _seS3SchemaVersion = "1.0"
                  }
                , _senUserIdentity = UserIdentityEntity "AWS:AHJD568HF4356HJJ:bob"
                }
              ]
            , _smMessageAttributes = mempty
            , _smMessageId         = "MessageId"
            , _smSignature         = "Signature"
            , _smSignatureVersion  = "1"
            , _smSigningCertUrl    = "https://sns.ap-southeast-2.amazonaws.com/SimpleNotificationService-my-cert.pem"
            , _smSubject           = "Amazon S3 Notification"
            , _smTimestamp         = UTCTime (fromGregorian 2019 11 1) 0
            , _smTopicArn          = "arn:aws:sns:ap-southeast-2:11111111111111:my-topic"
            , _smType              = "Notification"
            , _smUnsubscribeUrl    = "https://sns.ap-southeast-2.amazonaws.com/?Action=Unsubscribe&SubscriptionArn=arn:aws:sns:ap-southeast-2:11111111111111:my-topic:unsub"
          }
        , _sqsmAttributes        =
          [ ("ApproximateReceiveCount", "1")
          , ("SentTimestamp", "1572575717896")
          , ("SenderId", "AIDAIY4XCTD3OFZN5ED42")
          , ("ApproximateFirstReceiveTimestamp", "1572575717898")
          ]
        , _sqsmMessageAttributes =
          [ ( "Test"
            , MessageAttribute
              { _maType = "String"
              , _maValue = "TestString"
              })
          , ( "TestBinary"
            , MessageAttribute
              { _maType = "Binary"
              , _maValue = "TestBinary"
              })
          ]
        , _sqsmMd5OfBody         = "aca137746cb7d6a8e3eda3d1ce09b0c5"
        , _sqsmEventSource       = "aws:sqs"
        , _sqsmEventSourceARN    = "arn:aws:sqs:ap-southeast-2:1234567890:my-queue"
        , _sqsmAwsRegion         = Sydney
        }
      ]
