#!/bin/sh
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

# Root directory of the repository
DIST=$(cd $HERE/..; echo $PWD)

# Directory with the test project skeleton
SKELETON=$(cd $HERE/skeleton; echo $PWD)

# Directory with the expected outputs
EXPECTED=$(cd $HERE/expected; echo $PWD)

# Stackage resolver series to use
: "${RESOLVER_SERIES:=lts-12}"

# Find the latest resolver in the series to use.
RESOLVER=$(curl -s https://www.stackage.org/download/snapshots.json | \
               jq -r '."'$RESOLVER_SERIES'"')
echo "Using resolver: $RESOLVER"

# Extra dependencies to use for the resolver
EXTRA_DEPS_YAML=$HERE/extra-deps.$RESOLVER_SERIES
if [ -f $EXTRA_DEPS_YAML ]
then
    EXTRA_DEPS=$(cat $EXTRA_DEPS_YAML)
else
    EXTRA_DEPS=''
fi

# Temporary directory to create a project in
DIR=$(mktemp -d)
echo "Testing in $DIR"
if $DRY_RUN
then
    trap "rm -rf $DIR" EXIT
else
    trap "(sls --no-color remove || true); rm -rf $DIR" EXIT
fi
cd $DIR

NAME=s-h-test-$(pwgen 10 -0 -A)

# Copy the test files over, replacing the values
SED="sed s!NAME!$NAME!g;s!DIST!$DIST!g;s!RESOLVER!$RESOLVER!g;s!DOCKER!$DOCKER!g;s!EXTRA_DEPS!$EXTRA_DEPS!g"
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
sls package
echo "Packaging verified."

# Test local invocation
sls invoke local --function main --data '[4, 5, 6]' | \
    grep -v 'Serverless: ' > local_output.txt

diff $EXPECTED/local_output.txt local_output.txt && echo "Expected local result verified."

# Test serverless-offline
sls offline start --exec \
    "sh -c 'curl -s http://localhost:3000/hello/integration > offline_output.txt'"
diff $EXPECTED/offline_output.txt offline_output.txt && echo "Expected serverless-offline result verified."

if [ "$DRY_RUN" = "true" ]
then
    # All done (locally)
    exit 0
else
    # Deploy to AWS
    sls deploy

    # Run the function and verify the results
    sls invoke --function main --data '[4, 5, 6]' > output.json

    diff $EXPECTED/output.json output.json && echo "Expected result verified."

    # Wait for the logs to be propagated and verify them, ignoring volatile request
    # IDs and extra blank lines
    sleep 10
    sls logs --function main | grep -v RequestId | grep -v '^\W*$' > logs.txt

    diff $EXPECTED/logs.txt logs.txt && echo "Expected output verified."

    # Run the function from the subdirectory and verify the result
    sls invoke --function subdir --data '{}' > subdir_output.json

    diff $EXPECTED/subdir_output.json subdir_output.json && \
        echo "Expected result verified from subdir function."

    # Update a function
    sed 's/33/44/g' Main.hs
    sls deploy function --function hello

    # Verify the updated result
    sls invoke --function main --data '[4, 5, 6]' > output_modified.json

    diff $EXPECTED/output_modified.json output.json && echo "Expected updated result verified."
fi
