#!/usr/bin/env bash

# Fail on first error, on undefined variables, and on failures in pipelines.
set -euo pipefail

export CARGO_TARGET_DIR=/tmp/test_crates
RUSTDOC_OUTPUT_DIR="$CARGO_TARGET_DIR/doc"
TOPLEVEL="$(git rev-parse --show-toplevel)"
TARGET_DIR="$TOPLEVEL/localdata/test_data"

# Allow setting an explicit toolchain, like +nightly or +beta.
set +u
TOOLCHAIN="$1"
set -u
echo "Generating rustdoc with: $(cargo $TOOLCHAIN --version)"
RUSTDOC_CMD="cargo $TOOLCHAIN rustdoc"

# Run rustdoc on test_crates/*/
for crate_path in $(find "$TOPLEVEL/test_crates/" -maxdepth 1 -mindepth 1 -type d); do
    # Removing path prefix, leaving only the directory name without forward slashes
    crate=${crate_path#"$TOPLEVEL/test_crates/"}

    if [[ -f "$TOPLEVEL/test_crates/$crate/Cargo.toml" ]]; then
        echo "Generating: $crate"

        pushd "$TOPLEVEL/test_crates/$crate"
        RUSTC_BOOTSTRAP=1 $RUSTDOC_CMD -- -Zunstable-options --document-private-items --document-hidden-items --output-format=json
        mkdir -p "$TARGET_DIR/$crate"
        mv "$RUSTDOC_OUTPUT_DIR/$crate.json" "$TARGET_DIR/$crate/rustdoc.json"
        popd
    fi
done

unset CARGO_TARGET_DIR
