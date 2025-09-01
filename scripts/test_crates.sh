#!/usr/bin/env bash

# Fail on first error, on undefined variables, and on failures in pipelines.
set -euo pipefail

BASE_CARGO_TARGET_DIR=/tmp/test_crates
TOPLEVEL="$(git rev-parse --show-toplevel)"

JOBS=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)}

CRATES=$(find "$TOPLEVEL/test_crates" -maxdepth 1 -mindepth 1 -type d -exec basename {} \; | sort)

# The "feature_not_on_our_target_triple" crate is intentionally set up not to
# compile on x86_64 and aarch64 targets, so skip it here.
CRATES_TO_TEST=()
for crate in $CRATES; do
    if [[ "$crate" == "feature_not_on_our_target_triple" ]]; then
        continue
    fi
    CRATES_TO_TEST+=("$crate")
done

FAILED_CRATES_FILE="$BASE_CARGO_TARGET_DIR/failed"
rm -f "$FAILED_CRATES_FILE"

test_crate() {
    local crate="$1"
    local crate_target_dir="$BASE_CARGO_TARGET_DIR/$crate"
    local log="$crate_target_dir/output.log"
    mkdir -p "$crate_target_dir"
    (
        cd "$TOPLEVEL/test_crates/$crate"
        {
            echo "== $crate: cargo test --no-run =="
            CARGO_TARGET_DIR="$crate_target_dir" cargo test --no-run
            echo "== $crate: cargo test =="
            CARGO_TARGET_DIR="$crate_target_dir" cargo test
        } &>"$log"
    ) || echo "$crate" >>"$FAILED_CRATES_FILE"
}
export -f test_crate
export TOPLEVEL BASE_CARGO_TARGET_DIR FAILED_CRATES_FILE

printf '%s\n' "${CRATES_TO_TEST[@]}" | xargs -I{} -P "$JOBS" bash -c 'test_crate "$@"' _ {}

if [[ -f "$FAILED_CRATES_FILE" ]]; then
    echo
    echo "The following test crates failed:"
    while IFS= read -r crate; do
        echo "--- $crate ---"
        cat "$BASE_CARGO_TARGET_DIR/$crate/output.log"
        echo
    done <"$FAILED_CRATES_FILE"
    exit 1
fi
