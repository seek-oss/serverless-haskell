{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AWSLambda.Events.SNSEventSpec where

import           AWSLambda.Events.Records
import           AWSLambda.Events.SNSEvent

import           Data.Aeson
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Time.Calendar
import           Data.Time.Clock

import           Text.RawString.QQ

import           Test.Hspec

spec :: Spec
spec =
  describe "SNSEvent" $
    it "parses sample event" $
      decode sampleSNSJSON `shouldBe` Just sampleSNSEvent

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

sampleSNSEvent :: SNSEvent
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
