#!/usr/bin/env bash

# Top-level test runner. This runs all cabal (unit) tests, as well as separate
# integration tests.

function fail {
    echo -e "FAIL: $*" 1>&2
    exit 1
}

echo "Building MLSpec"
cabal build exe:MLSpec || fail "Couldn't build MLSpec"
echo "Successfully built MLSpec"

echo "Running MLSpec test suite"
cabal test --show-details=streaming || fail "cabal test failed"
echo "Successfully ran test suite"

echo "Running integration test executable"
cabal run mlspec-test-quickspec || fail "mlspec-test-quickspec failed"
echo "Successfully ran integration test"

echo "Running MLSpec on example data"
[[ "$(find test-data -type f -name "*format*" | wc -l)" -gt 0 ]] ||
    fail "Didn't find any formatted test-data"

for FILE in test-data/*format*
do
    echo "Checking '$FILE' is valid JSON"
    LENGTH=$(jq 'length' < "$FILE") ||
        fail "Couldn't read '$FILE' as JSON"

    EXPECT=$(basename "$FILE" | rev | cut -d '.' -f1 | rev)
    [[ "$EXPECT" -eq "$LENGTH" ]] ||
        fail "'$FILE' should have '$EXPECT' entries, found '$LENGTH'"

    echo "Running '$FILE' through MLSpec"
    OUTPUT=$(cabal run -v0 MLSpec < "$FILE") ||
        fail "MLSpec failed for stdin from '$FILE'"
    JSON=$(echo "$OUTPUT" | grep -v "^Depth")
    EQS=$(echo "$JSON" | jq -s 'length') ||
        fail "Found no JSON for stdin '$FILE'. Got '$JSON'"
    echo "Found '$EQS' equations from '$FILE'"

    echo "Running MLSpec on '$FILE'"
    OUTPUT=$(cabal run -v0 MLSpec "$FILE") ||
        fail "MLSpec failed for argument '$FILE'"
    JSON=$(echo "$OUTPUT" | grep -v "^Depth")
    EQS=$(echo "$JSON" | jq -s 'length') ||
        fail "Found no JSON for argument '$FILE'. Got '$JSON'"
    echo "Found '$EQS' equations from '$FILE'"
done
echo "Successfully processed example data"

echo "Running MLSpec on empty input"
OUTPUT=$(echo "[]" | cabal run -v0 MLSpec) ||
    fail "MLSpec failed on empty input"
echo "Successfully processed empty input"

echo "Running MLSpec on empty cluster"
OUTPUT=$(echo "[[]]" | cabal run -v0 MLSpec) ||
    fail "MLSpec failed on empty cluster"
echo "Successfully processed empty cluster"

echo "All tests pass"
