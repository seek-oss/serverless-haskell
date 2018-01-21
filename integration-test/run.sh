#!/bin/sh
# Test deploying a function to AWS and running it.

set -e

# Root directory of the repository
DIST=$(cd $(dirname $0)/..; echo $PWD)

# Directory with the integration test files
TEST=$(cd $(dirname $0); echo $PWD)

# Stackage resolver to use
RESOLVER=$(curl -s https://www.stackage.org/download/snapshots.json | jq -r .lts)

# Temporary directory to create a project in
DIR=$(mktemp -d)
echo "Testing in $DIR"
trap "(sls --no-color remove || true); rm -rf $DIR" EXIT
cd $DIR

NAME=s-h-test-$(pwgen 10 -0 -A)

mkdir $NAME
cd $NAME

SED="sed s!NAME!$NAME!g;s!DIST!$DIST!g;s!RESOLVER!$RESOLVER!g"
$SED < $TEST/package.json > package.json
$SED < $TEST/serverless.yml > serverless.yml
$SED < $TEST/package.yaml > package.yaml
$SED < $TEST/stack.yaml > stack.yaml
$SED < $TEST/Main.hs > Main.hs

export PATH=$(npm bin):$PATH

npm install serverless
npm install $DIST/serverless-plugin

sls --no-color deploy
sls --no-color invoke --function $NAME --data '[4, 5, 6]' > output.json

diff $TEST/output.json output.json && echo "Expected result verified."

sleep 10
sls --no-color logs --function $NAME | \
    grep -v RequestId > logs.txt

diff $TEST/logs.txt logs.txt && echo "Expected output verified."
