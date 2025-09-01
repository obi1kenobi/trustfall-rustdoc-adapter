#!/usr/bin/env bash

# Fail on first error, on undefined variables, and on failures in pipelines.
set -euo pipefail

BASE_CARGO_TARGET_DIR=/tmp/test_crates
TOPLEVEL="$(git rev-parse --show-toplevel)"

JOBS=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)}

CRATES=$(find "$TOPLEVEL/test_crates" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)

test_crate() {
    local crate="$1"
    local crate_target_dir="$BASE_CARGO_TARGET_DIR/$crate"
    echo "Testing: $crate"
    (
        cd "$TOPLEVEL/test_crates/$crate"
        CARGO_TARGET_DIR="$crate_target_dir" cargo test --no-run
        CARGO_TARGET_DIR="$crate_target_dir" cargo test
    )
}
export -f test_crate
export TOPLEVEL BASE_CARGO_TARGET_DIR

printf '%s\n' $CRATES | xargs -I{} -P "$JOBS" bash -c 'test_crate "$@"' _ {}
