{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AWSLambda.Events.SNSEventSpec where

import           AWSLambda.Events.Records
import           AWSLambda.Events.S3Event
import           AWSLambda.Events.SNSEvent

import           Data.Aeson
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Text                 (Text)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.AWS.S3            as S3

import           Text.RawString.QQ

import           Test.Hspec

spec :: Spec
spec =
  describe "SNSEvent" $ do
    it "parses sample text event" $
      decode sampleSNSJSON `shouldBe` Just sampleSNSEvent
    it "parses sample embedded S3 event" $
      decode sampleSNSS3JSON `shouldBe` Just sampleSNSS3Event

sampleSNSJSON :: ByteString
sampleSNSJSON = [r|
{
  "Records": [
    {
      "EventVersion": "1.0",
      "EventSubscriptionArn": "eventsubscriptionarn",
      "EventSource": "aws:sns",
      "Sns": {
        "SignatureVersion": "1",
        "Timestamp": "1970-01-01T00:00:00.000Z",
        "Signature": "EXAMPLE",
        "SigningCertUrl": "EXAMPLE",
        "MessageId": "95df01b4-ee98-5cb9-9903-4c221d41eb5e",
        "Message": "Hello from SNS!",
        "MessageAttributes": {
          "Test": {
            "Type": "String",
            "Value": "TestString"
          },
          "TestBinary": {
            "Type": "Binary",
            "Value": "TestBinary"
          }
        },
        "Type": "Notification",
        "UnsubscribeUrl": "EXAMPLE",
        "TopicArn": "topicarn",
        "Subject": "TestInvoke"
      }
    }
  ]
}
|]

sampleSNSEvent :: SNSEvent Text
sampleSNSEvent =
  RecordsEvent
    [ SNSRecord
      { _srEventVersion = "1.0"
      , _srEventSubscriptionArn = "eventsubscriptionarn"
      , _srEventSource = "aws:sns"
      , _srSns =
        SNSMessage
        { _smMessage = "Hello from SNS!"
        , _smMessageAttributes =
          HashMap.fromList
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
        , _smMessageId = "95df01b4-ee98-5cb9-9903-4c221d41eb5e"
        , _smSignature = "EXAMPLE"
        , _smSignatureVersion = "1"
        , _smSigningCertUrl = "EXAMPLE"
        , _smSubject = "TestInvoke"
        , _smTimestamp = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
        , _smTopicArn = "topicarn"
        , _smType = "Notification"
        , _smUnsubscribeUrl = "EXAMPLE"
        }
      }
    ]

sampleSNSS3JSON :: ByteString
sampleSNSS3JSON = [r|
{"Records":
  [
    {"EventSource":"aws:sns",
      "EventVersion":"1.0",
      "EventSubscriptionArn":"arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent:23e2d254-e8bb-4db5-92ce-d917e5aad090",
      "Sns":{
        "Type":"Notification",
        "MessageId":"89f6fe8b-a751-5dcd-8e0c-afbd75420455",
        "TopicArn":"arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent",
        "Subject":"Amazon S3 Notification",
        "Message": "{\"Records\":[{\"eventVersion\":\"2.0\",\"eventSource\":\"aws:s3\",\"awsRegion\":\"ap-southeast-2\",\"eventTime\":\"2017-03-06T02:56:19.713Z\",\"eventName\":\"ObjectCreated:Put\",\"userIdentity\":{\"principalId\":\"AWS:DFLKSDFLKJ987SDFLJJDJ:some-principal-id\"},\"requestParameters\":{\"sourceIPAddress\":\"192.168.0.1\"},\"responseElements\":{\"x-amz-request-id\":\"324098EDFLK0894F\",\"x-amz-id-2\":\"xsdSDF/pgAl401Fz3UIATJ5/didfljDSFDSFsdfkjsdfl8JdsfLSDF89ldsf7SDF898jsdfljiA=\"},\"s3\":{\"s3SchemaVersion\":\"1.0\",\"configurationId\":\"SomeS3Event:Created\",\"bucket\":{\"name\":\"some-bucket\",\"ownerIdentity\":{\"principalId\":\"A3O1SDFLKJIJXU\"},\"arn\":\"arn:aws:s3:::some-bucket\"},\"object\":{\"key\":\"path/to/some/object\",\"size\":53598442,\"eTag\":\"6b1f72b9e81e4d6fcd3e0c808e8477f8\",\"sequencer\":\"0058BCCFD25C798E7B\"}}}]}",
        "Timestamp":"2017-03-06T02:56:19.834Z",
        "SignatureVersion":"1",
        "Signature":"aybEgnTjKzSbC2puHxho7SUnYOje4SjBoCyt0Q13bMWyp7M64+EU6jzi7P01+gSIuBFyYPsHreSmyqGMRSxbFuzn7rG5JcVGN0901U3CRXdk42eh03je8evRvs/Oa7TJlhpCTEDDOScalCWbIH0RthYONQpPR01nEgaNKj3e8YVJqyRQV+4RbU3YWJOj+Spyi4u1hOC9PLUv4BH7U80nbhbOe9EwgX0zpeNU1WBRbEpqPoACm+7/uB0w79qFBKjB/Q7OWc1kASUZV9q8bz03yceoQeVvza0QGhPsnSXi49sn1mLWQOFS4KvgbJIC/Qk7H036ShrDioP6pP+UEg6kow==",
        "SigningCertUrl":"https://sns.ap-southeast-2.amazonaws.com/SimpleNotificationService-b95095beb82e8f6a046b3aafc7f4149a.pem",
        "UnsubscribeUrl":"https://sns.ap-southeast-2.amazonaws.com/?Action=Unsubscribe&SubscriptionArn=arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent:23e2d254-e8bb-4db5-92ce-d917e5aad090",
        "MessageAttributes":{}
      }
    }
  ]
}
|]

sampleSNSS3Event :: SNSEvent (Embedded S3Event)
sampleSNSS3Event =
  RecordsEvent
  { _reRecords =
    [ SNSRecord
      { _srEventVersion = "1.0"
      , _srEventSubscriptionArn =
        "arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent:23e2d254-e8bb-4db5-92ce-d917e5aad090"
      , _srEventSource = "aws:sns"
      , _srSns =
        SNSMessage
        { _smMessage =
          TextValue
          { _unTextValue =
            Embedded
            { _unEmbed =
              RecordsEvent
              { _reRecords =
                [ S3EventNotification
                  { _senAwsRegion = Sydney
                  , _senEventName = S3ObjectCreatedPut
                  , _senEventSource = "aws:s3"
                  , _senEventTime =
                    UTCTime
                      (fromGregorian 2017 3 6)
                      (picosecondsToDiffTime 10579713000000000)
                  , _senEventVersion = "2.0"
                  , _senRequestParameters =
                    RequestParametersEntity
                    { _rpeSourceIPAddress = "192.168.0.1"
                    }
                  , _senResponseElements =
                    ResponseElementsEntity
                    { _reeXAmzId2 =
                      "xsdSDF/pgAl401Fz3UIATJ5/didfljDSFDSFsdfkjsdfl8JdsfLSDF89ldsf7SDF898jsdfljiA="
                    , _reeXAmzRequestId = "324098EDFLK0894F"
                    }
                  , _senS3 =
                    S3Entity
                    { _seBucket =
                      S3BucketEntity
                      { _sbeArn = "arn:aws:s3:::some-bucket"
                      , _sbeName = BucketName "some-bucket"
                      , _sbeOwnerIdentity =
                        UserIdentityEntity
                        { _uiePrincipalId = "A3O1SDFLKJIJXU"
                        }
                      }
                    , _seConfigurationId = "SomeS3Event:Created"
                    , _seObject =
                      S3ObjectEntity
                      { _soeETag =
                        Just (ETag "6b1f72b9e81e4d6fcd3e0c808e8477f8")
                      , _soeKey = ObjectKey "path/to/some/object"
                      , _soeSize = Just 53598442
                      , _soeSequencer = "0058BCCFD25C798E7B"
                      , _soeVersionId = Nothing
                      }
                    , _seS3SchemaVersion = "1.0"
                    }
                  , _senUserIdentity =
                    UserIdentityEntity
                    { _uiePrincipalId =
                      "AWS:DFLKSDFLKJ987SDFLJJDJ:some-principal-id"
                    }
                  }
                ]
              }
            }
          }
        , _smMessageAttributes = HashMap.fromList []
        , _smMessageId = "89f6fe8b-a751-5dcd-8e0c-afbd75420455"
        , _smSignature =
          "aybEgnTjKzSbC2puHxho7SUnYOje4SjBoCyt0Q13bMWyp7M64+EU6jzi7P01+gSIuBFyYPsHreSmyqGMRSxbFuzn7rG5JcVGN0901U3CRXdk42eh03je8evRvs/Oa7TJlhpCTEDDOScalCWbIH0RthYONQpPR01nEgaNKj3e8YVJqyRQV+4RbU3YWJOj+Spyi4u1hOC9PLUv4BH7U80nbhbOe9EwgX0zpeNU1WBRbEpqPoACm+7/uB0w79qFBKjB/Q7OWc1kASUZV9q8bz03yceoQeVvza0QGhPsnSXi49sn1mLWQOFS4KvgbJIC/Qk7H036ShrDioP6pP+UEg6kow=="
        , _smSignatureVersion = "1"
        , _smSigningCertUrl =
          "https://sns.ap-southeast-2.amazonaws.com/SimpleNotificationService-b95095beb82e8f6a046b3aafc7f4149a.pem"
        , _smSubject = "Amazon S3 Notification"
        , _smTimestamp =
          UTCTime
            (fromGregorian 2017 3 6)
            (picosecondsToDiffTime 10579834000000000)
        , _smTopicArn = "arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent"
        , _smType = "Notification"
        , _smUnsubscribeUrl =
          "https://sns.ap-southeast-2.amazonaws.com/?Action=Unsubscribe&SubscriptionArn=arn:aws:sns:ap-southeast-2:012345678901:SomeSNSEvent:23e2d254-e8bb-4db5-92ce-d917e5aad090"
        }
      }
    ]
  }
