{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module: AWSLambda.Events.SQSEvent
Description: Types for SQS Lambda events
-}
module AWSLambda.Events.SQSEvent where

import           Control.Exception.Safe (MonadCatch)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON(..), genericParseJSON)
import           Data.Aeson.Casing (aesonPrefix, camelCase)
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Amazonka.Data.Base64
import           Amazonka.Data.Text (FromText)
import qualified Amazonka.Types as AWS

import           AWSLambda.Events.MessageAttribute
import           AWSLambda.Events.Records
import           AWSLambda.Handler (lambdaMain)

data SQSMessage body = SQSMessage
  { _sqsmMessageId         :: !Text
  , _sqsmReceiptHandle     :: !Text
  , _sqsmBody              :: !(TextValue body)
  , _sqsmAttributes        :: !(HashMap Text Text)
  , _sqsmMessageAttributes :: !(HashMap Text MessageAttribute)
  , _sqsmMd5OfBody         :: !Text
  , _sqsmEventSource       :: !Text
  , _sqsmEventSourceARN    :: !Text
  , _sqsmAwsRegion         :: !AWS.Region
  } deriving (Show, Eq, Generic)

instance FromText message => FromJSON (SQSMessage message) where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

$(makeLenses ''SQSMessage)

type SQSEvent body = RecordsEvent (SQSMessage body)

-- | A Traversal to get messages from an SQSEvent
sqsMessages :: Traversal (SQSEvent message) (SQSEvent message') message message'
sqsMessages = reRecords . traverse . sqsmBody . unTextValue

-- | A Traversal to get embedded JSON values from an SQSEvent
sqsEmbedded :: Traversal (SQSEvent (Embedded v)) (SQSEvent (Embedded v')) v v'
sqsEmbedded = sqsMessages . unEmbed

sqsBinary :: Traversal' (SQSEvent Base64) ByteString
sqsBinary = sqsMessages . _Base64

-- | Traverse all the messages in an SQS event
traverseSqs :: (FromJSON a, Applicative m) => (a -> m ()) -> SQSEvent (Embedded a) -> m ()
traverseSqs act = traverseRecords $ \record ->
    act $ record ^. sqsmBody . unTextValue . unEmbed

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual SQS messages
sqsMain :: (FromJSON a, MonadCatch m, MonadIO m) => (a -> m ()) -> m ()
sqsMain = lambdaMain . traverseSqs
