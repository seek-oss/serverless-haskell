{-# LANGUAGE OverloadedStrings #-}
module ApiGW where

import           AWSLambda
import           AWSLambda.Events.APIGateway
import           Control.Lens hiding (Context)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text)

handler :: APIGatewayProxyRequest Text -> Context -> IO (Either String (APIGatewayProxyResponse Text))
handler request context = do
  putStrLn "This should go to logs"
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name ->
      pure (Right $ responseOK & responseBody ?~ "Hello, " <> name)

    Nothing ->
      pure (Right responseNotFound)
