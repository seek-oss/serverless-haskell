{-|
Module      : AWSLambda.Handler
Stability   : experimental
Portability : POSIX

Tools for running Haskell on AWS Lambda using Serverless.

= Usage

To deploy a Haskell function on AWS Lambda:

* Initialise a Serverless project in the same directory as your Stack-enabled
  package and install the @serverless-haskell@ plugin:

  > npm init .
  > npm install --save serverless serverless-haskell

* Create @serverless.yml@ with the following contents:

  > provider:
  >   name: aws
  >   runtime: nodejs6.10
  >
  > functions:
  >   myfunc:
  >     handler: mypackage.mypackage-exe
  >     # Here, mypackage is the Haskell package name and mypackage-exe is the
  >     # executable name as defined in the Cabal file
  >
  > plugins:
  >   - serverless-haskell

* Write your @main@ function using 'AWSLambda.lambdaMain'.

* Use @sls deploy@ to deploy the executable to AWS Lambda. __Note__: @sls deploy
  function@ is not supported.

  The `serverless-haskell` plugin will build the package using Stack and upload
  it to AWS together with a JavaScript wrapper to pass the input and output
  from/to AWS Lambda.

  You can test the function and see the invocation results with `sls invoke
  myfunc`.

= Additional features

Configuration options are passed to the plugin under @haskell@ key in @custom@
section of @serverless.yml@.

* To add flags to @stack build@, specify them as an array under
  @stackBuildArgs@:

  > custom:
  >   haskell:
  >     stackBuildArgs:
  >       - --pedantic
  >       - --allow-different-user

* To start the executable with extra arguments, add them to @arguments@ under
  the function name:

  > custom:
  >   haskell:
  >     arguments:
  >       myfunc:
  >         - --arg1
  >         - --arg2
  >         - arg3

* To include dependent libraries, list them under @extraLibraries@. The
  libraries will be uploaded to AWS Lambda along with the executable.

  > custom:
  >   haskell:
  >     extraLibraries:
  >       - libpcre.so.3
-}
module AWSLambda
  ( Handler.lambdaMain
  , module AWSLambda.Events
  ) where

import qualified AWSLambda.Handler as Handler

import AWSLambda.Events
