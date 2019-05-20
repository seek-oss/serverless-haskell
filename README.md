# Serverless Haskell

[![Build status](https://img.shields.io/travis/seek-oss/serverless-haskell.svg)](https://travis-ci.org/seek-oss/serverless-haskell)
[![Hackage](https://img.shields.io/hackage/v/serverless-haskell.svg)](https://hackage.haskell.org/package/serverless-haskell)
[![Stackage LTS](https://www.stackage.org/package/serverless-haskell/badge/lts)](https://www.stackage.org/lts/package/serverless-haskell)
[![Hackage dependencies](https://img.shields.io/hackage-deps/v/serverless-haskell.svg)](https://packdeps.haskellers.com/feed?needle=serverless-haskell)
[![npm](https://img.shields.io/npm/v/serverless-haskell.svg)](https://www.npmjs.com/package/serverless-haskell)

Deploying Haskell code onto [AWS Lambda] using [Serverless].

## Prerequisites

* AWS account
* [Stack]
* [NPM]
* [Docker]

## Usage

There are two ways to start, either via the stack template, or directly modifying a project. You may want to use the manual approach as the template specifies a specific stack resolver as it needs to hardcode the `stack.yaml` file.

In either case, you will want to have [Serverless] installed, eg. `npm install -g serverless`.

### Using the stack template

* Create a [Stack] package for your code:

  ```shell
  stack new mypackage https://raw.githubusercontent.com/seek-oss/serverless-haskell/master/serverless-haskell.hsfiles
  ```

* Update the resolver in the `stack.yaml` file. This is hardcoded as the resolver number is not known at template interpolation time. You should pick either the latest resolver, or one you have used before and have thus prebuilt many of the core packages for.

* Install the dependencies and build the project:

  ```shell
  cd mypackage
  npm install
  stack build
  sls invoke local -f mypackage-func
  ```

  This should invoke serverless locally and display output once everything has built.

### Manually

* Create a [Stack] package for your code:

  ```shell
  stack new mypackage
  ```

  LTS 9-13 are supported, older versions are likely to work too but untested.

* Initialise a Serverless project inside the Stack package directory and install
  the `serverless-haskell` plugin:

  ```shell
  cd mypackage
  npm init .
  npm install --save serverless serverless-haskell@x.y.z
  ```

  The version of the NPM package to install must match the version of the
  Haskell package.

* Create `serverless.yml` with the following contents:

  ```yaml
  service: myservice

  provider:
    name: aws
    runtime: provided

  functions:
    myfunc:
      handler: src/Lib.handler
      layers:
        - arn:aws:lambda:us-east-1:785355572843:layer:haskell-runtime:3

  plugins:
    - serverless-haskell
  ```

* Write your `handler` function at `src/Lib.hs`:

  ```haskell
  import qualified Data.Aeson as Aeson

  import AWSLambda
  import qualified Data.Aeson as Aeson

  handler :: Aeson.Value -> Context -> IO (Either String [Int])
  handler evt = do
    putStrLn "This should go to logs"
    print evt
    return (Right [1, 2, 3])
  ```

* Configure your project with the runtime at `app/Main.hs`:

  ```haskell
  module Main where

  import qualified Lib

  import AWSLambda

  configureLambda
  ```

* Use `sls deploy` to deploy the executable to AWS Lambda.

  The `serverless-haskell` plugin will build the package using Stack and upload
  it to AWS.

  You can test the function and see the invocation results with
  `sls invoke -f myfunc`.

  To invoke the function locally, use `sls invoke local -f myfunc`.

### API Gateway

This plugin supports handling API Gateway requests. Declare the HTTP events
normally in `serverless.yml` and use
[AWSLambda.Events.APIGateway](https://hackage.haskell.org/package/serverless-haskell/docs/AWSLambda-Events-APIGateway.html)
in the handler to process them.

[Serverless Offline] can be used for local testing of API Gateway requests.

### Notes

* Only AWS Lambda is supported at the moment. Other cloud providers would
  require different JavaScript wrappers to be implemented.

See
[AWSLambda](https://hackage.haskell.org/package/serverless-haskell/docs/AWSLambda.html)
for documentation, including additional options to control the deployment.

## Development

Currently, apart from the `master` branch, an `v0.6.x` branch is maintained with
no breaking changes since 0.6.0, for inclusion into Stackage LTS 12. For
bugfixes and new features that do not change the existing behavior, please
target `v0.6.x`. For anything else, target `master`.

Changes to the LTS branch will be forward ported to the `master` branch after
releasing.

### Testing

* Haskell code is tested with Stack: `stack test`.
* JavaScript code is linted with `eslint`.

### Integration tests

Integration test verifies that the project can build and deploy a complete
function to AWS, and it runs with expected functionality.

Integration test is only automatically run up to deployment due to the need for
an AWS account. To run manually:

* Ensure you have the required dependencies: `curl`, [jq], [NPM], `pwgen` and
  [Stack].
* Get an AWS account and add the access credentials into your shell environment.
* Run `./integration-test/run.sh`. The exit code indicates success.
* To verify just the packaging, without deployment, run
  `./integration-test/run.sh --dry-run`.
* By default, the integration test is run with LTS 13. To specify a different
  series, use `RESOLVER_SERIES=lts-9`.
* To avoid creating a temporary directory for every run, specify
  `--no-clean-dir`. This can speed up repeated test runs, but does not guarantee
  the same results as a clean test.

### Releasing

* Ensure you are on a correct branch (`v0.6.x` or `master`).
* Ensure that all the changes are reflected in the changelog.
* Run the integration tests.
* Run `./bumpversion major|minor|patch`. This will increment the version number,
  update the changelog, create and push the Git tag and the branch.
* If you have released an LTS version, merge the version branch into `master`,
  taking care of the conflicts around version numbers and changelog, and release
  the latest version as well.

[AWS Lambda]: https://aws.amazon.com/lambda/
[Docker]: https://www.docker.com/
[jq]: https://stedolan.github.io/jq/
[NPM]: https://www.npmjs.com/
[Serverless]: https://serverless.com/framework/
[Serverless Offline]: https://github.com/dherault/serverless-offline
[Stack]: https://haskellstack.org
