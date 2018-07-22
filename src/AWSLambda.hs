{-|
Module      : AWSLambda
Stability   : experimental
Portability : POSIX

Tools for running Haskell on AWS Lambda using Serverless.

= Usage

To deploy a Haskell function on AWS Lambda:

* Initialise a Serverless project in the same directory as your Stack-enabled
package and install the @serverless-haskell@ plugin:

    > npm init .
    > npm install --save serverless serverless-haskell@x.y.z

    The version of the NPM package to install must match the version of the
    Haskell package.

* Create @serverless.yml@ with the following contents:

    > service: myservice
    >
    > provider:
    >   name: aws
    >   runtime: nodejs8.10
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
function@ is
<https://github.com/seek-oss/serverless-haskell/issues/20 not supported yet>.

    The @serverless-haskell@ plugin will build the package using Stack and upload
    it to AWS together with a JavaScript wrapper to pass the input and output
    from/to AWS Lambda.

    You can test the function and see the invocation results with @sls invoke
    myfunc@.

    To invoke the function locally, use @sls invoke local -f myfunc@.

= API Gateway

This plugin supports handling API Gateway requests. Declare the HTTP events
normally in @serverless.yml@ and use 'AWSLambda.Events.APIGateway' in the
handler to process them.

<https://github.com/dherault/serverless-offline Serverless Offline> can be used
for local testing of API Gateway requests.

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

* Dependent system libraries not present in the AWS Lambda environment will be
automatically uploaded along with the executable. Note that while statically
linking the executable via Cabal options is possible, it might still require
the corresponding glibc version on the AWS environment.

* To force using Stack's Docker image, set @docker@ key to @true@. It is
recommended to set this to avoid incompatibility issues with dependent system
libraries.

    > custom:
    >   haskell:
    >     docker: true
-}
module AWSLambda
  ( Handler.lambdaMain
  , module AWSLambda.Events
  ) where

import qualified AWSLambda.Handler as Handler

import AWSLambda.Events
