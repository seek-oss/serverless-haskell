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

import           Control.Lens            hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing       (aesonDrop, camelCase)
import           Data.Aeson.TH           (deriveFromJSON)
-- import           Data.CaseInsensitive (CI (..))
import           Data.Aeson.Embedded
import           Data.Aeson.TextValue
import           Data.Aeson.Types        (Parser)
import           Data.ByteString         (ByteString)
import qualified Data.CaseInsensitive    as CI
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text, splitOn)
import qualified Data.Text               as Text
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Text.Read          (decimal)
import           Data.Word               (Word8)
import           GHC.Generics            (Generic)
import           Network.AWS.Data.Base64
import           Network.AWS.Data.Text
import qualified Network.HTTP.Types      as HTTP

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
  , _riSourceIp                      :: !(Word8, Word8, Word8, Word8)
  , _riCognitoAuthenticationType     :: !(Maybe Text)
  , _riCognitoAuthenticationProvider :: !(Maybe Text)
  , _riUserArn                       :: !(Maybe Text)
  , _riUserAgent                     :: !(Maybe Text)
  , _riUser                          :: !(Maybe Text)
  } deriving (Eq, Show)

instance FromJSON RequestIdentity where
  parseJSON = withObject "RequestIdentity" $ \o ->
    RequestIdentity
    <$> o .:? "cognitoIdentityPoolId"
    <*> o .:? "accountId"
    <*> o .:? "cognitoIdentityId"
    <*> o .:? "caller"
    <*> o .:? "apiKey"
    <*> (parseIP =<< o.: "sourceIp")
    <*> o.:? "cognitoAuthenticationType"
    <*> o.:? "cognitoAuthenticationProvider"
    <*> o.:? "userArn"
    <*> o.:? "userAgent"
    <*> o.:? "user"
    where
      parseIP :: Text -> Parser (Word8, Word8, Word8, Word8)
      parseIP t =
        case splitOn "." t of
          [a, b, c, d] -> either fail pure $ do
            a' <- fst <$> decimal a
            b' <- fst <$> decimal b
            c' <- fst <$> decimal c
            d' <- fst <$> decimal d
            pure (a', b', c', d')
          _ -> fail "String not IP format"
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
  , _prcProtocol     :: !Text
  } deriving (Eq, Show)
$(deriveFromJSON (aesonDrop 4 camelCase) ''ProxyRequestContext)
$(makeLenses ''ProxyRequestContext)

data APIGatewayProxyRequest body = APIGatewayProxyRequest
  { _agprqResource              :: !Text
  , _agprqPath                  :: !ByteString
  , _agprqHttpMethod            :: !HTTP.Method
  , _agprqHeaders               :: !HTTP.RequestHeaders
  , _agprqQueryStringParameters :: !HTTP.Query
  , _agprqPathParameters        :: !(HashMap PathParamName PathParamValue)
  , _agprqStageVariables        :: !(HashMap StageVarName StageVarValue)
  , _agprqRequestContext        :: !ProxyRequestContext
  , _agprqBody                  :: !(Maybe (TextValue body))
  } deriving (Eq, Show, Generic)

instance FromText body => FromJSON (APIGatewayProxyRequest body) where
  parseJSON = withObject "APIGatewayProxyRequest" $ \o ->
    APIGatewayProxyRequest
    <$> o .: "resource"
    <*> (encodeUtf8 <$> o .: "path")
    <*> (encodeUtf8 <$> o .: "httpMethod")
    <*> (fmap fromAWSHeaders <$> o .:? "headers") .!= mempty
    <*> (fmap fromAWSQuery <$> o .:? "queryStringParameters") .!= mempty
    <*> o .:? "pathParameters" .!= HashMap.empty
    <*> o .:? "stageVariables" .!= HashMap.empty
    <*> o .: "requestContext"
    <*> o .:? "body"
    where
      -- Explicit type signatures so that we don't accidentally tell Aeson
      -- to try to parse the wrong sort of structure
      fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
      fromAWSHeaders = fmap toHeader . HashMap.toList
        where
          toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8
      fromAWSQuery :: HashMap QueryParamName QueryParamValue -> HTTP.Query
      fromAWSQuery = fmap toQueryItem . HashMap.toList
        where
          toQueryItem = bimap encodeUtf8 (\x -> if Text.null x then Nothing else Just . encodeUtf8 $ x)

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
  , _agprsHeaders    :: !HTTP.ResponseHeaders
  , _agprsBody       :: !(Maybe (TextValue body))
  } deriving (Eq, Show, Generic)

instance ToText body => ToJSON (APIGatewayProxyResponse body) where
  toJSON APIGatewayProxyResponse {..} =
    object
      [ "statusCode" .= _agprsStatusCode
      , "headers" .= toAWSHeaders _agprsHeaders
      , "body" .= _agprsBody
      ]
    where
      toAWSHeaders :: HTTP.ResponseHeaders -> HashMap HeaderName HeaderValue
      toAWSHeaders = HashMap.fromList . fmap (bimap (decodeUtf8 . CI.original) decodeUtf8)

instance FromText body => FromJSON (APIGatewayProxyResponse body) where
  parseJSON =
    withObject "APIGatewayProxyResponse" $ \o ->
      APIGatewayProxyResponse <$> o .: "statusCode" <*>
      (fromAWSHeaders <$> o .: "headers") <*>
      o .:? "body"
      -- Explicit type signatures so that we don't accidentally tell Aeson
      -- to try to parse the wrong sort of structure
    where
      fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
      fromAWSHeaders = fmap toHeader . HashMap.toList
        where
          toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

$(makeLenses ''APIGatewayProxyResponse)

response :: Int -> APIGatewayProxyResponse body
response statusCode = APIGatewayProxyResponse statusCode mempty Nothing

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
