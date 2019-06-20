{-# LANGUAGE OverloadedStrings #-}
module ApiGateway where

import AWSLambda
import AWSLambda.Events.APIGateway
import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup
import Data.Text (Text)

handler :: APIGatewayProxyRequest Text -> AWSLambda.Context -> IO (Either Text (APIGatewayProxyResponse Text))
handler request _ = do
  putStrLn "This should go to logs"
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name -> return . Right $ responseOK & responseBody ?~ "Hello, " <> name <> "\n"
    Nothing -> return $ Right responseNotFound
