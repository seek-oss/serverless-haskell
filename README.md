# Serverless Haskell

Deploying Haskell code onto [AWS Lambda] using [Serverless].

## Usage

* Initialise a Serverless project in the same directory as your Stack-enabled
  package and install the `serverless-haskell` plugin:

  ```shell
  npm init .
  npm install --save serverless serverless-haskell
  ```

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
[AWSLambda](https://hackage.haskell.org/package/serverless-haskell/docs/AWSLambda.html)
for the documentation.

## Releasing

* Install [bumpversion](https://github.com/peritus/bumpversion): `pip install bumpversion`.
* Run `bumpversion major|minor|patch`.
* Run `git push --tags && git push`.

[AWS Lambda]: https://aws.amazon.com/lambda/
[Serverless]: https://serverless.com/framework/
