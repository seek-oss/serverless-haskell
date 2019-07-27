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
    >   runtime: haskell
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

* Use @sls deploy@ to deploy the executable to AWS Lambda.

    The @serverless-haskell@ plugin will build the package using Stack and upload
    it to AWS together with a JavaScript wrapper to pass the input and output
    from/to AWS Lambda.

    You can test the function and see the invocation results with @sls invoke
    -f myfunc@.

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

* Stack's Docker image will be used to match the AWS Lambda Linux environment.
To disable this, set @docker@ key to @false@, but beware that the resulting
binary might not have the required libraries to run on Lambda.

    > custom:
    >   haskell:
    >     docker: false

* By default, @stack build@ command is invoked to build all the project's
executables. To only build the ones used in the handlers, set @buildAll@ key to
@false@. Note that at least Stack 1.9.3 has better caching behavior when 
building the whole project, as it doesn't need to reconfigure the build for the
individual ones every time.

    > custom:
    >   haskell:
    >     buildAll: false

-}
module AWSLambda
  ( Handler.lambdaMain
  ) where

import qualified AWSLambda.Handler as Handler
