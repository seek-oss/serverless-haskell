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

# Remove volatile bits from the output
sanitise() {
  grep -v 'Serverless: ' | \
  grep -v RequestId | \
  grep -v '^[[:space:]]*$' | \
  sed '/Error ----/{a\
<Serverless error omitted>
q
}'
}

# Test that output of the command, save for volatile bits, is as expected
assert_output() {
    MESSAGE="$1"
    shift
    FILE="$1"
    shift
    "$@" > $FILE
    # Trim volatile content
    cat $FILE | sanitise > stable-$FILE
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

# Test that output of the command contains expected text
assert_contains_output() {
    MESSAGE="$1"
    shift
    OUTPUT="$1"
    shift
    OUTFILE=$(mktemp)
    "$@" > $OUTFILE
    # Trim volatile content
    cat $OUTFILE | sanitise > $OUTFILE-stable
    if grep -q --fixed-strings "$OUTPUT" $OUTFILE-stable
    then
        rm $OUTFILE $OUTFILE-stable
        assert_success "$MESSAGE" true
    else
        echo -e "${RED}Unexpected output from '$*':${NC}"
        cat $OUTFILE
        echo -e "${RED}Expected:${NC}"
        echo $OUTPUT
        rm $OUTFILE $OUTFILE-stable
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
