#!/bin/bash
# Test packaging a function, deploying it to AWS and running it. With --dry-run,
# only packaging is tested. With --no-docker, Docker isn't used for packaging.

set -e

DOCKER=true
while [ $# -gt 0 ]
do
    case "$1" in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --no-docker)
            DOCKER=false
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
    which $DEPENDENCY >/dev/null || \
        (echo "$DEPENDENCY is required for the test." >&2; exit 1)
done

# Directory of the integration test
HERE=$(dirname $0)

. $HERE/tests.sh

# Root directory of the repository
DIST=$(cd $HERE/..; echo $PWD)

# Directory with the test project skeleton
SKELETON=$(cd $HERE/skeleton; echo $PWD)

# Stackage resolver series to use
: "${RESOLVER_SERIES:=lts-13}"

# Find the latest resolver in the series to use.
# RESOLVER=$(curl -s https://www.stackage.org/download/snapshots.json | \
#                jq -r '."'$RESOLVER_SERIES'"')
RESOLVER="lts-13.25" # somehow 13.26 is not available
echo "Using resolver: $RESOLVER"

# Extra dependencies to use for the resolver
EXTRA_DEPS_YAML=$HERE/extra-deps.$RESOLVER_SERIES
if [ -f $EXTRA_DEPS_YAML ]
then
    EXTRA_DEPS=$(cat $EXTRA_DEPS_YAML)
else
    EXTRA_DEPS=''
fi

if [ -n "$REUSE_DIR" ]
then
    DIR=$HERE/run
    mkdir -p $DIR
    echo "Testing in $DIR"
    if $DRY_RUN
    then
        :
    else
        trap "(sls --no-color remove || true)" EXIT
    fi

    NAME=s-h-test
else
    # Temporary directory to create a project in
    DIR=$(mktemp -d)
    echo "Testing in $DIR"
    if $DRY_RUN
    then
        trap "rm -rf $DIR" EXIT
    else
        trap "(sls --no-color remove || true); rm -rf $DIR" EXIT
    fi

    NAME=s-h-test-$(pwgen 10 -0 -A)
fi
cd $DIR

# Copy the test files over, replacing the values
SED="sed s!NAME!$NAME!g;s!DIST!$DIST!g;s!RESOLVER!$RESOLVER!g;s!DOCKER_DEFAULT!$DOCKER!g;s!EXTRA_DEPS!$EXTRA_DEPS!g"
for FILE in $(find $SKELETON -type f | grep -v /\\. | sed "s!$SKELETON/!!")
do
    mkdir -p $(dirname $FILE)
    $SED < $SKELETON/$FILE > $FILE
done

export PATH=$(npm bin):$PATH

# Install Serverless
npm install serverless
npm install $DIST/serverless-plugin
npm install serverless-offline

# Just package the service first
assert_success "sls package" sls package

# Test packaging without Docker
FORCE_DOCKER=false sls package > no_docker_sls_package.txt
assert_success "custom variable disables Docker" \
                grep -q "Serverless: Warning: not using Docker to build" no_docker_sls_package.txt

# Test local invocation
sls invoke local --function main --data '[4, 5, 6]' | \
    grep -v 'Serverless: ' > local_output.txt

assert_file_same "sls invoke local" local_output.txt

# Test local invocation of a JavaScript function
sls invoke local --function jsfunc --data '{}' | \
    grep -v 'Serverless: ' > local_output_js.txt

assert_file_same "sls invoke local (JavaScript)" local_output_js.txt

# Test serverless-offline
sls offline start --providedRuntime --exec \
    "sh -c 'curl -s http://localhost:3000/hello/integration > offline_output.txt'"

assert_file_same "sls offline" offline_output.txt

if [ "$DRY_RUN" = "true" ]
then
    # All done (locally)
    :
else
    # Deploy to AWS
    sls deploy

    # Run the function and verify the results
    sls invoke --function main --data '[4, 5, 6]' > output.json

    assert_file_same "sls invoke" output.json

    # Wait for the logs to be propagated and verify them, ignoring volatile request
    # IDs and extra blank lines
    sleep 20
    sls logs --function main | grep -v RequestId | grep -v '^\W*$' > logs.txt

    assert_file_same "sls logs" logs.txt

    # Run the function from the subdirectory and verify the result
    sls invoke --function subdir --data '{}' > subdir_output.json

    assert_file_same "sls invoke (subdirectory)" subdir_output.json

    # Run the JavaScript function and verify the results
    sls invoke --function jsfunc --data '[4, 5, 6]' > output_js.json

    assert_file_same "sls invoke (JavaScript)" output_js.json

    # Update a function
    sed 's/33/44/g' Main.hs > Main_modified.hs && mv Main_modified.hs Main.hs
    sls deploy function --function main

    # Verify the updated result
    sls invoke --function main --data '[4, 5, 6]' > output_modified.json

    assert_file_same "sls invoke (after sls deploy function)" output_modified.json
fi

end_tests
