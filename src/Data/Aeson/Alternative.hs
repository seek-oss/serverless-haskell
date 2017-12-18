{-|
Module      :  Data.Aeson.Alternative
Stability   : experimental

Utilities for decoding JSON into one of the possible types and handling the
resulting sum type.
-}
module Data.Aeson.Alternative
  ( AlternativeJSON
  , alternative
  ) where

import Control.Applicative

import Data.Aeson

-- | One of the two values that has been parsed from JSON
data AlternativeJSON a b
  = FirstJSON a
  | SecondJSON b
  deriving (Eq, Ord, Show)

instance (FromJSON a, FromJSON b) => FromJSON (AlternativeJSON a b) where
  parseJSON v = FirstJSON <$> parseJSON v <|> SecondJSON <$> parseJSON v

-- | Handle either of the two types that have been parsed from JSON
alternative :: (a -> r) -> (b -> r) -> AlternativeJSON a b -> r
alternative f _ (FirstJSON a) = f a
alternative _ g (SecondJSON b) = g b
