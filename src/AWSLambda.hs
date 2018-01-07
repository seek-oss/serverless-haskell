{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Tools for running Haskell on AWS Lambda.

= Usage

To deploy a Haskell function on AWS Lambda:

* Initialise a Serverless project in the same directory as your Stack-enabled
  package and install the @serverless-haskell@ plugin:

  > npm init .
  > npm install --save serverless serverless-haskell

* Add the following to @serverless.yml@:

  > provider:
  >   name: aws
  >   runtime: nodejs6.10
  >
  > functions:
  >   myfunc:
  >     handler: mypackage.myfunc
  >     # Here, mypackage is the Haskell package name and myfunc is the executable
  >     # name as defined in the Cabal file
  >
  > plugins:
  >   - serverless-haskell

* Write your @main@ function using 'AWSLambda.lambdaMain'.

* Use @sls deploy@ to deploy the executable to AWS Lambda. __Note__: @sls deploy
  function@ is not supported.

= Additional features

To add flags to @stack build@, add the following key to @serverless.yml@:

> custom:
>   haskell:
>     stackBuildArgs:
>       - --pedantic
>       - --allow-different-user
-}
module AWSLambda
  ( Handler.lambdaMain
  , module AWSLambda.Events
  ) where

import qualified AWSLambda.Handler as Handler

import AWSLambda.Events
