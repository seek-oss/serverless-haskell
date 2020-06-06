{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AWSLambda.Events.Records where

import           Control.Exception.Safe (MonadCatch)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON(..), withObject, (.:))
import           Data.Foldable (traverse_)

import           AWSLambda.Handler (lambdaMain)

newtype RecordsEvent a = RecordsEvent { _reRecords :: [a] } deriving (Eq, Show, Functor, Foldable)

instance FromJSON a => FromJSON (RecordsEvent a) where
  parseJSON = withObject "RecordsEvent" $ \o -> RecordsEvent <$> o .: "Records"

$(makeLenses ''RecordsEvent)

-- | Traverse all the records in a Lambda event
traverseRecords :: Applicative m => (a -> m ()) -> RecordsEvent a -> m ()
traverseRecords = traverse_

-- | A specialised version of the 'lambdaMain' entry-point
-- for handling individual records in a Lambda event
recordsMain :: (FromJSON a, MonadCatch m, MonadIO m) => (a -> m ()) -> m ()
recordsMain = lambdaMain . traverseRecords
