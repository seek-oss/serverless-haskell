{-|
Main module for the integration test.
-}
{-# LANGUAGE TemplateHaskell #-}

import AWSLambda

import qualified SubDir.SubModule
import qualified ApiGateway
import qualified ThirdParty

generateLambdaDispatcher
