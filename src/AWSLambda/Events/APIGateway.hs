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

-- | A Traversal to get the request body, if ther is one.
requestBody :: Traversal (APIGatewayProxyRequest body) (APIGatewayProxyRequest body') body body'
requestBody = agprqBody . _Just . unTextValue

requestBodyEmbedded :: Traversal (APIGatewayProxyRequest (Embedded v)) (APIGatewayProxyRequest (Embedded v')) v v'
requestBodyEmbedded = requestBody . unEmbed

requestBodyBinary :: Traversal' (APIGatewayProxyRequest Base64) ByteString
requestBodyBinary = requestBody . _Base64

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

responseOK :: body -> APIGatewayProxyResponse body
responseOK body =
  APIGatewayProxyResponse
  { _agprsStatusCode = 200
  , _agprsHeaders = HashMap.empty
  , _agprsBody = Just (TextValue body)
  }

responseNotFound :: APIGatewayProxyResponse body
responseNotFound =
  APIGatewayProxyResponse
  { _agprsStatusCode = 404
  , _agprsHeaders = HashMap.empty
  , _agprsBody = Nothing
  }

-- | Specialisation of lambdaMain for API Gateway
apiGatewayMain
  :: (FromText reqBody, ToText resBody)
  => (APIGatewayProxyRequest reqBody -> IO (APIGatewayProxyResponse resBody)) -- ^ Function to process the event
  -> IO ()
apiGatewayMain = lambdaMain
