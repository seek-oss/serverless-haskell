module Data.Aeson.TestUtil where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import Test.Hspec

testEncodeDecode :: (FromJSON a, ToJSON a, Show a, Eq a) => String -> ByteString -> a -> Spec
testEncodeDecode description bytestring value = describe description $ do
        it "decodes" $
          decode bytestring `shouldBe` Just value
        it "encodes" $
          decode (encode value) `shouldBe` Just value
