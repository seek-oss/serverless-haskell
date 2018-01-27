{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.EmbeddedSpec where

import Data.Aeson
import Data.Aeson.Embedded

import Test.Hspec

spec :: Spec
spec = describe "Embedded" $ do
  let test d b v = describe d $ do
        it "decodes" $
          decode b `shouldBe` Just (Embedded v)
        it "encodes" $
          encode (Embedded v) `shouldBe` b
  test "array" "\"[1,2,3]\"" [1::Int, 2, 3]
  test "string" "\"\\\"foo\\\"\"" ("foo"::String)
  test "object" "\"{\\\"a\\\":42,\\\"b\\\":true}\"" (object ["a" .= (42::Int), "b" .= True])
