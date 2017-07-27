#!/usr/bin/env bash

function fail {
    echo "FAIL: $1" >> /dev/stderr
    exit 1
}

cabal build || fail "Couldn't cabal build"

function runBenchmarks {
    while read -r E
    do
        command time -f "{\"benchmark\":\"$E\", \"maxResKB\": %M}" \
                ./dist/build/"$E/$E" || fail "Failed to benchmark $E"
    done < <(grep executable < MLSpec.cabal | grep bench | grep -o "bench-.*")
}

RESULTS=$(runBenchmarks 3>&2 2>&1 1>&3)  # Show stdout, store stderr (times)

DATA=$(echo "$RESULTS" | grep "^{" | jq -s '.')

MAX_KB=$(echo "$DATA" | jq 'map(.maxResKB) | max')
MAX_NM=$(echo "$DATA" | jq 'map(.benchmark | length) | max | . + 1')

echo ""
echo "Maximum resident set size: ${MAX_KB}kb"
echo ""

while read -r ENTRY
do
    NAME=$(echo "$ENTRY" | jq -r '.benchmark')

    BAR=$(echo "$ENTRY" |
          jq --argjson maxnm "$MAX_NM" \
             --argjson cols "$COLUMNS" \
             --argjson maxkb "$MAX_KB" \
             -r '"#" * ((($cols - 21) * .maxResKB) / $maxkb)')

    printf "%-20s %s\n" "$NAME" "$BAR"
done < <(echo "$DATA" | jq -c '.[]')
