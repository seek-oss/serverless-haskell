#!/bin/bash
# Test packaging a function, deploying it to AWS and running it. With --dry-run,
# only packaging is tested. With --no-docker, Docker isn't used for packaging.

set -euo pipefail

DRY_RUN=
REUSE_DIR=
FAILFAST=
while [ $# -gt 0 ]
do
  case "$1" in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --no-clean-dir)
      REUSE_DIR=true
      shift
      ;;
    --failfast)
      FAILFAST=true
      shift
      ;;
    *)
      shift
      ;;
  esac
done

for DEPENDENCY in curl jq npm pwgen stack
do
  command -v $DEPENDENCY >/dev/null || \
    (echo "$DEPENDENCY is required for the test." >&2; exit 1)
done
if command -v pkgconf >/dev/null
then
  PKGCONF=pkgconf
elif command -v pkg-config >/dev/null
then
  PKGCONF=pkg-config
else
  echo "pkg-config is required for the test." >&2
  exit 1
fi
for DEPENDENCY in libpcre
do
  $PKGCONF --libs $DEPENDENCY >/dev/null || \
    (echo "$DEPENDENCY is required for the test." >&2; exit 1)
done

# Directory of the integration test
HERE=$(cd $(dirname $0); echo $PWD)

. $HERE/tests.sh

# Root directory of the repository
DIST=$(cd $HERE/..; echo $PWD)

# Directory with the test project skeleton
SKELETON=$(cd $HERE/skeleton; echo $PWD)

# Stackage resolver series to use
: "${RESOLVER_SERIES:=$(cat $DIST/stack.yaml | grep resolver | sed -E 's/resolver: (lts-[0-9]+)\..+/\1/')}"

SLS_OFFLINE_PID=
function kill_sls_offline () {
  if [ -n "$SLS_OFFLINE_PID" ] && kill -0 $SLS_OFFLINE_PID
  then
    kill $SLS_OFFLINE_PID || true
    SLS_OFFLINE_PID=
  fi
}
function cleanup () {
  kill_sls_offline
  if [ -z "$DRY_RUN" ]
  then
    sls --no-color remove || true
  fi
  if [ -z "$REUSE_DIR" ]
  then
    rm -rf $DIR
  fi
}
trap cleanup exit

if [ -n "$REUSE_DIR" ]
then
    DIR=$HERE/run
    mkdir -p $DIR
    echo "Testing in $DIR"

    NAME=s-h-test
else
    # Temporary directory to create a project in
    DIR=$(mktemp -d)
    echo "Testing in $DIR"

    NAME=s-h-test-$(pwgen 10 -0 -A)
fi
cd $DIR

# Make sure test directory is accessible by Docker containers
chmod +rx $DIR
umask u=rwx,g=rx,o=rx

RESOLVER=$($DIST/latest-lts $RESOLVER_SERIES)
echo "Using resolver: $RESOLVER"

# Extra dependencies to use for the resolver
EXTRA_DEPS=$HERE/extra-deps.$RESOLVER_SERIES
if ! [ -f $EXTRA_DEPS ]
then
    EXTRA_DEPS=/dev/null
fi

# Copy the test files over, replacing the values
skeleton() {
    mkdir -p $(dirname $1)
    sed "s!NAME!$NAME!g
s!DIST!$DIST!g
s!RESOLVER!$RESOLVER!g
/EXTRA_DEPS/{
r$EXTRA_DEPS
d
}" < $SKELETON/$1 > $1
}
for FILE in $(find $SKELETON -type f | grep -v /\\. | sed "s!$SKELETON/!!")
do
    skeleton $FILE
done

export PATH=$(npm bin):$PATH

# Install Serverless and serverless-offline
npm install serverless
npm install serverless-offline

# Compile and install the plugin
pushd $DIST >/dev/null
npm install
find . -maxdepth 1 -type f -name "serverless-haskell-*.tgz" -delete
npm pack
popd >/dev/null
npm install $DIST/serverless-haskell-*.tgz

# Just package the service first
assert_success "sls package" sls package

# Test local invocation
assert_expected_output "sls invoke local" local_output.txt \
    sls invoke local --function main --data '[4, 5, 6]'

# Test local invocation that errors
assert_expected_output "sls invoke local" local_error_output.txt \
    sh -c 'sls invoke local --function main --data '"'"'{"error":1}'"'"' || true'

# Test local invocation of a JavaScript function
assert_expected_output "sls invoke local (JavaScript)" local_output_js.txt \
    sls invoke local --function jsfunc --data '{}'

# Test serverless-offline
sls offline start --useDocker &
SLS_OFFLINE_PID=$!
until curl http://localhost:3002/ >/dev/null 2>&1
do
    sleep 1
done
assert_expected_output "sls offline" offline_output.txt \
    curl -s http://localhost:3000/dev/hello/integration

kill_sls_offline

if [ -n "$DRY_RUN" ]
then
    # All done (locally)
    :
else
    # Deploy to AWS
    sls deploy

    # Run the function and verify the results
    assert_expected_output "sls invoke" output.json \
        sls invoke --function main --data '[4, 5, 6]'

    # Wait for the logs to be propagated and verify them
    sleep 20
    assert_expected_output "sls logs" logs.txt \
        sls logs --function main

    # Run the function a few times in repetition
    assert_expected_output "sls invoke (multiple)" multi_output.txt \
        bash -c "for i in {1..10}; do sls invoke --function main --data []; done"

    # Run the function from the subdirectory and verify the result
    assert_expected_output "sls invoke (subdirectory)" subdir_output.json \
        sls invoke --function subdir --data '{}'

    # Run the JavaScript function and verify the results
    assert_expected_output "sls invoke (JavaScript)" output_js.json \
        sls invoke --function jsfunc --data '[4, 5, 6]'

    # Update a function
    sed 's/33/44/g' Main.hs > Main_modified.hs && mv Main_modified.hs Main.hs
    sls deploy function --function main

    # Verify the updated result
    assert_expected_output "sls invoke (after sls deploy function)" output_modified.json \
        sls invoke --function main --data '[4, 5, 6]'
fi

end_tests
