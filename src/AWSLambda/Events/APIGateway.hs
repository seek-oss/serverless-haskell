{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module AWSLambda.Events.APIGateway where

import           Control.Lens.TH
import           Data.Aeson.Casing         (aesonDrop, camelCase)
import           Data.Aeson.TH             (deriveFromJSON)
-- import           Data.CaseInsensitive (CI (..))
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

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
  { _riCognitoIdentityPoolId         :: !Text
  , _riAccountId                     :: !Text
  , _riCognitoIdentityId             :: !Text
  , _riCaller                        :: !Text
  , _riApiKey                        :: !Text
  , _riSourceIp                      :: !Text
  , _riCognitoAuthenticationType     :: !Text
  , _riCognitoAuthenticationProvider :: !Text
  , _riUserArn                       :: !Text
  , _riUserAgent                     :: !Text
  , _riUser                          :: !Text
  } deriving (Eq, Show)

$(deriveFromJSON (aesonDrop 3 camelCase) ''RequestIdentity)
$(makeLenses ''RequestIdentity)

data ProxyRequestContext = ProxyRequestContext
  { _prcPath         :: !Text
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

data APIGatewayProxyRequest = APIGatewayProxyRequest
  { _agprqResource              :: !Text
  , _agprqPath                  :: !Text
  , _agprqHttpMethod            :: !Method
  , _agprqHeaders               :: !(HashMap HeaderName HeaderValue)
  , _agprqQueryStringParameters :: !(HashMap QueryParamName QueryParamValue)
  , _agprqPathParameters        :: !(HashMap PathParamName PathParamValue)
  , _agprqStageVariables        :: !(HashMap StageVarName StageVarValue)
  , _agprqRequestContext        :: !ProxyRequestContext
  , _agprqBody                  :: !Text
  , _agprqIsBase64Encoded       :: !Bool
  } deriving (Eq, Show)
$(deriveFromJSON (aesonDrop 6 camelCase) ''APIGatewayProxyRequest)
$(makeLenses ''APIGatewayProxyRequest)
