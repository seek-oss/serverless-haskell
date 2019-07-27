module Main where

import AWSLambda.Handler

import Data.Aeson.Alternative

main :: IO ()
main = lambdaMain $ handlerIntArray `alternative` handlerString

handlerIntArray :: [Int] -> IO String
handlerIntArray values = do
  putStrLn "This should go to logs"
  pure $ "Highest value is " ++ show (maximum values)

handlerString :: String -> IO String
handlerString _ = pure "String received"
