{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

{-|
Module: AWSLambda.Events.S3Event
Description: Types for S3 Lambda events

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.S3Events
-}
module AWSLambda.Events.S3Event where

import           Control.Lens.TH
import           Control.Monad            (guard)
import           Data.Aeson               (FromJSON (..), withObject, (.:))
import           Data.Aeson.Casing        (aesonDrop, camelCase)
import           Data.Aeson.TH            (deriveFromJSON)
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           Amazonka.S3              (BucketName, ETag, Event(..), ObjectKey, ObjectVersionId)
import qualified Amazonka.Types           as AWS

import           AWSLambda.Events.Records
import           AWSLambda.Orphans        ()

newtype UserIdentityEntity = UserIdentityEntity
  { _uiePrincipalId :: Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 4 camelCase) ''UserIdentityEntity)
$(makeLenses ''UserIdentityEntity)

data S3BucketEntity = S3BucketEntity
  { _sbeArn           :: !Text
  , _sbeName          :: !BucketName
  , _sbeOwnerIdentity :: !UserIdentityEntity
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 4 camelCase) ''S3BucketEntity)
$(makeLenses ''S3BucketEntity)

data S3ObjectEntity = S3ObjectEntity
  { _soeETag      :: !(Maybe ETag)
  , _soeKey       :: !ObjectKey
  , _soeSize      :: !(Maybe Integer)
  , _soeSequencer :: !Text
  , _soeVersionId :: !(Maybe ObjectVersionId)
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 4 camelCase) ''S3ObjectEntity)
$(makeLenses ''S3ObjectEntity)

newtype RequestParametersEntity = RequestParametersEntity
  { _rpeSourceIPAddress :: Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 4 camelCase) ''RequestParametersEntity)
$(makeLenses ''RequestParametersEntity)

data ResponseElementsEntity = ResponseElementsEntity
  { _reeXAmzId2       :: !Text
  , _reeXAmzRequestId :: !Text
  } deriving (Eq, Show)

instance FromJSON ResponseElementsEntity where
  parseJSON =
    withObject "ResponseElementsEntity" $
    \o ->
       ResponseElementsEntity <$> o .: "x-amz-id-2" <*> o .: "x-amz-request-id"
$(makeLenses ''ResponseElementsEntity)

data S3Entity = S3Entity
  { _seBucket          :: !S3BucketEntity
  , _seConfigurationId :: !Text
  , _seObject          :: !S3ObjectEntity
  , _seS3SchemaVersion :: !Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 camelCase) ''S3Entity)
$(makeLenses ''S3Entity)

data S3EventNotification = S3EventNotification
  { _senAwsRegion         :: !AWS.Region
  , _senEventName         :: !Event
  , _senEventSource       :: !Text
  , _senEventTime         :: !UTCTime
  , _senEventVersion      :: !Text
  , _senRequestParameters :: !RequestParametersEntity
  , _senResponseElements  :: !ResponseElementsEntity
  , _senS3                :: !S3Entity
  , _senUserIdentity      :: !UserIdentityEntity
  } deriving (Eq, Show)

instance FromJSON S3EventNotification where
  parseJSON = withObject "S3EventNotification" $ \o -> do
    _senEventSource <- o .: "eventSource"
    guard $ _senEventSource == "aws:s3"
    _senAwsRegion <- o .: "awsRegion"
    _senEventName <- o .: "eventName"
    _senEventTime <- o .: "eventTime"
    _senEventVersion <- o .: "eventVersion"
    _senRequestParameters <- o .: "requestParameters"
    _senResponseElements <- o .: "responseElements"
    _senS3 <- o .: "s3"
    _senUserIdentity <- o .: "userIdentity"
    return S3EventNotification {..}
$(makeLenses ''S3EventNotification)

type S3Event = RecordsEvent S3EventNotification

-- | Is the event an object creation event
isCreateEvent :: S3EventNotification -> Bool
isCreateEvent e = case _senEventName e of
  Event_S3_ObjectCreated_CompleteMultipartUpload -> True
  Event_S3_ObjectCreated_Copy -> True
  Event_S3_ObjectCreated_Post -> True
  Event_S3_ObjectCreated_Put -> True
  Event_S3_ObjectCreated__ -> True
  Event_S3_ObjectRemoved_Delete -> False
  Event_S3_ObjectRemoved_DeleteMarkerCreated -> False
  Event_S3_ObjectRemoved__ -> False
  Event_S3_ObjectRestore_Completed -> False
  Event_S3_ObjectRestore_Post -> False
  Event_S3_ObjectRestore__ -> False
  Event_S3_ReducedRedundancyLostObject -> False
  Event_S3_Replication_OperationFailedReplication -> False
  Event_S3_Replication_OperationMissedThreshold -> False
  Event_S3_Replication_OperationNotTracked -> False
  Event_S3_Replication_OperationReplicatedAfterThreshold -> False
  Event_S3_Replication__ -> False
  Event' _ -> False


-- | Is the event an object removal event
isRemoveEvent :: S3EventNotification -> Bool
isRemoveEvent e = case _senEventName e of
  Event_S3_ObjectCreated_CompleteMultipartUpload -> False
  Event_S3_ObjectCreated_Copy -> False
  Event_S3_ObjectCreated_Post -> False
  Event_S3_ObjectCreated_Put -> False
  Event_S3_ObjectCreated__ -> False
  Event_S3_ObjectRemoved_Delete -> True
  Event_S3_ObjectRemoved_DeleteMarkerCreated -> True
  Event_S3_ObjectRemoved__ -> True
  Event_S3_ObjectRestore_Completed -> False
  Event_S3_ObjectRestore_Post -> False
  Event_S3_ObjectRestore__ -> False
  Event_S3_ReducedRedundancyLostObject -> False
  Event_S3_Replication_OperationFailedReplication -> False
  Event_S3_Replication_OperationMissedThreshold -> False
  Event_S3_Replication_OperationNotTracked -> False
  Event_S3_Replication_OperationReplicatedAfterThreshold -> False
  Event_S3_Replication__ -> False
  Event' _ -> False
