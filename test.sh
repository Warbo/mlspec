#!/usr/bin/env bash

function fail {
    echo "FAIL: $1" >> /dev/stderr
    exit 1
}

cabal build exe:MLSpec || fail "Couldn't build MLSpec"
cabal test --show-details=streaming || fail "cabal test failed"

rm -f *.tix

cabal run mlspec-test-quickspec || fail "mlspec-test-quickspec failed"

[[ "$(find test-data -type f -name "*format*" | wc -l)" -gt 0 ]] ||
    fail "Didn't find any formatted test-data"

for FILE in test-data/*format*
do
    OUTPUT=$(cabal run MLSpec < "$FILE") || fail "MLSpec failed for '$FILE'"
    echo "$OUTPUT" | grep "^{" > /dev/null || fail "Found no JSON for '$FILE'"
done
