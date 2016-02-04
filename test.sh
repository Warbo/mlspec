#!/usr/bin/env bash

# Top-level test runner. This runs all cabal (unit) tests, as well as separate
# integration tests.

function fail {
    echo "FAIL: $1" >> /dev/stderr
    exit 1
}

echo "Building MLSpec"
cabal build exe:MLSpec || fail "Couldn't build MLSpec"
echo "Successfully built MLSpec"

echo "Running MLSpec test suite"
cabal test --show-details=streaming || fail "cabal test failed"
echo "Successfully ran test suite"

rm -f *.tix

echo "Running integration test executable"
cabal run mlspec-test-quickspec || fail "mlspec-test-quickspec failed"
echo "Successfully ran integration test"

echo "Running MLSpec on example data"
[[ "$(find test-data -type f -name "*format*" | wc -l)" -gt 0 ]] ||
    fail "Didn't find any formatted test-data"

for FILE in test-data/*format*
do
    OUTPUT=$(cabal run MLSpec < "$FILE") || fail "MLSpec failed for '$FILE'"
    echo "$OUTPUT" | grep "^{" > /dev/null || fail "Found no JSON for '$FILE'"
done
echo "Successfully processed example data"
