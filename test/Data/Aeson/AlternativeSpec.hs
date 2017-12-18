{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.AlternativeSpec where

import Data.Aeson
import Data.Aeson.Alternative

import Test.Hspec

spec :: Spec
spec =
  describe "AlternativeJSON" $ do
    let handler1 :: [Int] -> Int
        handler1 = sum
    let handler2 :: Bool -> Int
        handler2 = fromEnum
    let handler = handler1 `alternative` handler2
    it "parses the first alternative" $
      (handler <$> decode "[1, 2, 3]") `shouldBe` (Just 6)
    it "parses the second alternative" $
      (handler <$> decode "true") `shouldBe` (Just 1)
    it "fails to parse anything else" $
      (handler <$> decode "{}") `shouldBe` Nothing
