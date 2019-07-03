module IntArray where

import AWSLambda

handler :: [Int] -> Context -> IO (Either String String)
handler values context = do
  putStrLn "This should go to logs"
  pure (Right $ "Highest value is " ++ show (maximum values))
