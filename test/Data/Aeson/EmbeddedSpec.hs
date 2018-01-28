{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.EmbeddedSpec where

import Data.Aeson
import Data.Aeson.Embedded
import Data.Aeson.TestUtil

import Test.Hspec

spec :: Spec
spec = describe "Embedded" $ do
  testEncodeDecode "array" "\"[1,2,3]\"" $ Embedded [1::Int, 2, 3]
  testEncodeDecode "string" "\"\\\"foo\\\"\"" $ Embedded ("foo"::String)
  testEncodeDecode "object" "\"{\\\"a\\\":42,\\\"b\\\":true}\"" $ Embedded $ object ["a" .= (42::Int), "b" .= True]
  testEncodeDecode "in field" "{\"a\":\"[1,2,3]\"}" $ object ["a" .= Embedded [1::Int, 2, 3]]
