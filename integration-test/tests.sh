#!/bin/bash
# Utility functions for tests

# Test running utilities
TESTS=0
FAILED=0

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Verify that a command succeeds
assert_success() {
    MESSAGE="$1"
    shift

    ((++TESTS))

    if "$@"
    then
        echo -e "${GREEN}$MESSAGE: success${NC}"
    else
        echo -e "${RED}${MESSAGE}: fail${NC}"
        ((++FAILED))
        if [ "$FAILFAST" = "true" ]
        then
            echo -e "${RED}Aborting further tests.${NC}"
            exit $FAILED
        fi
    fi
}

# Directory with the expected outputs
EXPECTED=$(cd $(dirname $0)/expected; echo $PWD)

# Test that output of the command, save for volatile bits, is as expected
assert_expected_output() {
    MESSAGE="$1"
    shift
    FILE="$1"
    shift
    "$@" > $FILE
    # Trim volatile content
    cat $FILE | grep -v 'Serverless: ' | grep -v RequestId | grep -v '^[[:space:]]*$' > stable-$FILE
    if diff -q $EXPECTED/$FILE stable-$FILE
    then
        assert_success "$MESSAGE" true
    else
        echo -e "${RED}Unexpected output from '$*':${NC}"
        cat $FILE
        echo -e "${RED}Difference:${NC}"
        diff $EXPECTED/$FILE stable-$FILE || true
        assert_success "$MESSAGE" false
    fi
}

# End testing and indicate the error code
end_tests() {
    if ((FAILED > 0))
    then
        echo -e "${RED}Run ${TESTS} tests, ${FAILED} failed.${NC}"
        exit $FAILED
    else
        echo -e "${GREEN}${TESTS} tests passed.${NC}"
        exit 0
    fi
}
