#!/bin/sh
# Test deploying a function to AWS and running it.

set -e

DIST=$(cd $(dirname $0); echo $PWD)

DIR=$(mktemp -d)
echo "Testing in $DIR"
trap "rm -rf $DIR" EXIT
cd $DIR

NAME=s-h-test-$(pwgen 10 -0 -A)

mkdir $NAME
cd $NAME

cat > package.json <<END
{
  "name": "$NAME",
  "version": "1.0.0"
}
END
export PATH=$(npm bin):$PATH

npm install serverless
npm install $DIST/serverless-plugin

cat > serverless.yml <<END
service: $NAME

provider:
  name: aws
  runtime: nodejs6.10

functions:
  $NAME:
    handler: $NAME.main

plugins:
- serverless-haskell
END

cat > package.yaml <<END
name: $NAME
version: 0.0.0

dependencies:
- base >= 4.7 && < 5
- aeson
- serverless-haskell

executables:
  main:
    main: Main.hs
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
END

cat > stack.yaml <<END
resolver: lts-10.3

packages:
- .
- location: $DIST
  extra-dep: true
END

cat > Main.hs <<END
import qualified Data.Aeson as Aeson

import AWSLambda

main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  print evt
  pure [1, 2, 3]
END

sls --no-color deploy
sls --no-color invoke --function $NAME --data '[4, 5, 6]'
sleep 10
sls --no-color logs --function $NAME
sls --no-color remove
