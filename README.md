# Serverless Haskell

Deploying Haskell code onto [AWS Lambda] using [Serverless].

## Usage

* Initialise a Serverless project in the same directory as your Stack-enabled
  package.

* Install `serverless-haskell` plugin (_Warning_: not uploaded to NPM registry
  yet, install manually by cloning this repository and specifying its
  `serverless-plugin` directory to `npm install`).

* Add the following to `serverless.yml`:

  ```yaml
  provider:
    name: aws
    runtime: nodejs6.10

  functions:
    myfunc:
      handler: mypackage.myfunc
      # Here, mypackage is the Haskell package name and myfunc is the executable
      # name as defined in the Cabal file

  plugins:
    - serverless-haskell
  ```

* Write your `main` function:

  ```haskell
  import qualified Data.Aeson as Aeson

  import AWSLambda

  main = lambdaMain handler

  handler :: Aeson.Value -> IO [Int]
  handler evt = do
    putStrLn "This should go to logs"
    print evt
    pure [1, 2, 3]
  ```

* Use `sls deploy` to deploy the executable to AWS Lambda. **Note**: `sls deploy
  function` is not supported.

See
[AWSLambda](https://github.com/SEEK-oss/serverless-haskell/blob/master/src/AWSLambda.hs)
for the documentation.

[AWS Lambda]: https://aws.amazon.com/lambda/
[Serverless]: https://serverless.com/framework/
