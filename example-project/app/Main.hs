module Main where

import AWSLambda.Handler
import AWSLambda.Events.S3Event

import Data.Aeson.Alternative

main :: IO ()
main = lambdaMain $ handlerIntArray `alternative` handlerS3

handlerIntArray :: [Int] -> IO String
handlerIntArray values = do
  putStrLn "This should go to logs"
  pure $ "Highest value is " ++ show (maximum values)

handlerS3 :: S3Event -> IO String
handlerS3 _ = pure "S3 event received"
