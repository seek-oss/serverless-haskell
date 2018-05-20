{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AWSLambda.Orphans where

import           Data.Aeson
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import           Network.AWS.Data.Text (fromText)
import qualified Network.AWS.S3        as S3

deriving instance FromJSON S3.BucketName

deriving instance FromJSON S3.ObjectKey

instance FromJSON S3.ETag where
  parseJSON = withText "ETag" $ either fail return . fromText

instance FromJSON S3.Event where
  parseJSON = withText "Event" $ either fail return . fromText . addS3Prefix
    where
      s3Prefix = "s3:"
      addS3Prefix s =
        if s3Prefix `Text.isPrefixOf` s
          then s
          else s3Prefix <> s
