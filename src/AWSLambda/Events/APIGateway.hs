{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module: AWSLambda.Events.APIGateway
Description: Types for APIGateway Lambda requests and responses

Based on https://github.com/aws/aws-lambda-dotnet/tree/master/Libraries/src/Amazon.Lambda.APIGatewayEvents
-}
module AWSLambda.Events.APIGateway where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing       (aesonDrop, camelCase)
import           Data.Aeson.TH           (deriveFromJSON)
-- import           Data.CaseInsensitive (CI (..))
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.ByteString         (ByteString)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Network.AWS.Data.Base64
import           Network.AWS.Data.Text

import           AWSLambda.Handler       (lambdaMain)

type Method = Text
-- type HeaderName = CI Text
type HeaderName = Text --- XXX should be CI Text
type HeaderValue = Text
type QueryParamName = Text
type QueryParamValue = Text
type PathParamName = Text
type PathParamValue = Text
type StageVarName = Text
type StageVarValue = Text

data RequestIdentity = RequestIdentity
  { _riCognitoIdentityPoolId         :: !(Maybe Text)
  , _riAccountId                     :: !(Maybe Text)
  , _riCognitoIdentityId             :: !(Maybe Text)
  , _riCaller                        :: !(Maybe Text)
  , _riApiKey                        :: !(Maybe Text)
  , _riSourceIp                      :: !(Maybe Text)
  , _riCognitoAuthenticationType     :: !(Maybe Text)
  , _riCognitoAuthenticationProvider :: !(Maybe Text)
  , _riUserArn                       :: !(Maybe Text)
  , _riUserAgent                     :: !(Maybe Text)
  , _riUser                          :: !(Maybe Text)
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 camelCase) ''RequestIdentity)
$(makeLenses ''RequestIdentity)

data ProxyRequestContext = ProxyRequestContext
  { _prcPath         :: !(Maybe Text)
  , _prcAccountId    :: !Text
  , _prcResourceId   :: !Text
  , _prcStage        :: !Text
  , _prcRequestId    :: !Text
  , _prcIdentity     :: !RequestIdentity
  , _prcResourcePath :: !Text
  , _prcHttpMethod   :: !Text
  , _prcApiId        :: !Text
  } deriving (Eq, Show)
$(deriveFromJSON (aesonDrop 4 camelCase) ''ProxyRequestContext)
$(makeLenses ''ProxyRequestContext)

data APIGatewayProxyRequest body = APIGatewayProxyRequest
  { _agprqResource              :: !Text
  , _agprqPath                  :: !Text
  , _agprqHttpMethod            :: !Method
  , _agprqHeaders               :: !(HashMap HeaderName HeaderValue)
  , _agprqQueryStringParameters :: !(HashMap QueryParamName QueryParamValue)
  , _agprqPathParameters        :: !(HashMap PathParamName PathParamValue)
  , _agprqStageVariables        :: !(HashMap StageVarName StageVarValue)
  , _agprqRequestContext        :: !ProxyRequestContext
  , _agprqBody                  :: !(Maybe (TextValue body))
  } deriving (Eq, Show, Generic)

instance FromText body => FromJSON (APIGatewayProxyRequest body) where
  parseJSON = withObject "APIGatewayProxyRequest" $ \o ->
    APIGatewayProxyRequest
    <$> o .: "resource"
    <*> o .: "path"
    <*> o .: "httpMethod"
    <*> o .:? "headers" .!= HashMap.empty
    <*> o .:? "queryStringParameters" .!= HashMap.empty
    <*> o .:? "pathParameters" .!= HashMap.empty
    <*> o .:? "stageVariables" .!= HashMap.empty
    <*> o .: "requestContext"
    <*> o .:? "body"


$(makeLenses ''APIGatewayProxyRequest)

-- | Get the request body, if there is one
requestBody :: Getter (APIGatewayProxyRequest body) (Maybe body)
requestBody = agprqBody . mapping unTextValue

-- | Get the embedded request body, if there is one
requestBodyEmbedded :: Getter (APIGatewayProxyRequest (Embedded v)) (Maybe v)
requestBodyEmbedded = requestBody . mapping unEmbed

-- | Get the binary (decoded Base64) request body, if there is one
requestBodyBinary :: Getter (APIGatewayProxyRequest Base64) (Maybe ByteString)
requestBodyBinary = requestBody . mapping _Base64

data APIGatewayProxyResponse body = APIGatewayProxyResponse
  { _agprsStatusCode :: !Int
  , _agprsHeaders    :: !(HashMap HeaderName HeaderValue)
  , _agprsBody       :: !(Maybe (TextValue body))
  } deriving (Eq, Show, Generic)

instance ToText body => ToJSON (APIGatewayProxyResponse body) where
  toJSON = genericToJSON $ aesonDrop 6 camelCase

instance FromText body => FromJSON (APIGatewayProxyResponse body) where
  parseJSON = genericParseJSON $ aesonDrop 6 camelCase

$(makeLenses ''APIGatewayProxyResponse)

response :: Int -> APIGatewayProxyResponse body
response statusCode = APIGatewayProxyResponse statusCode HashMap.empty Nothing

responseOK :: APIGatewayProxyResponse body
responseOK = response 200

responseNotFound :: APIGatewayProxyResponse body
responseNotFound = response 404

responseBadRequest :: APIGatewayProxyResponse body
responseBadRequest = response 400

responseBody :: Setter' (APIGatewayProxyResponse body) (Maybe body)
responseBody = agprsBody . at () . mapping unTextValue

responseBodyEmbedded :: Setter' (APIGatewayProxyResponse (Embedded body)) (Maybe body)
responseBodyEmbedded = responseBody . mapping unEmbed

responseBodyBinary :: Setter' (APIGatewayProxyResponse Base64) (Maybe ByteString)
responseBodyBinary = responseBody . mapping _Base64

-- | Process incoming events from @serverless-haskell@ using a provided
-- function.
--
-- This is a specialisation of lambdaMain for API Gateway.
--
-- The handler receives the input event given to the AWS Lambda function, and
-- its return value is returned from the function.
--
-- This is intended to be used as @main@, for example:
--
-- > import AWSLambda.Events.APIGateway
-- > import Control.Lens
-- > import Data.Aeson
-- > import Data.Aeson.Embedded
-- >
-- > main = apiGatewayMain handler
-- >
-- > handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
-- > handler request = do
-- >   putStrLn "This should go to logs"
-- >   print $ request ^. requestBody
-- >   pure $ responseOK & responseBodyEmbedded ?~ [1, 2, 3]
--
-- The type parameters @reqBody@ and @resBody@ represent the types of request and response body, respectively.
-- The @FromText@ and @ToText@ contraints are required because these values come from string fields
-- in the request and response JSON objects.
-- To get direct access to the body string, use @Text@ as the parameter type.
-- To treat the body as a stringified embedded JSON value, use @Embedded a@, where @a@ has the
-- appropriate @FromJSON@ or @ToJSON@ instances.
-- To treat the body as base 64 encoded binary use @Base64@.
apiGatewayMain
  :: (FromText reqBody, ToText resBody)
  => (APIGatewayProxyRequest reqBody -> IO (APIGatewayProxyResponse resBody)) -- ^ Function to process the event
  -> IO ()
apiGatewayMain = lambdaMain
