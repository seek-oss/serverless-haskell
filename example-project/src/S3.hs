module S3 where

import AWSLambda
import AWSLambda.Events.S3Event

handlerS3 :: S3Event -> Context -> IO (Either String String)
handlerS3 _ _ =
  pure (Right "S3 event received")
