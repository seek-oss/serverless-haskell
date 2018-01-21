{-|
Main module for the integration test.
-}
import qualified Data.Aeson as Aeson

import AWSLambda

import System.Environment

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  -- Test logs going through
  putStrLn "This should go to logs"
  -- Test passed arguments
  getArgs >>= print
  -- Test passed event
  print evt
  -- Test return value
  pure [1, 2, 3]
