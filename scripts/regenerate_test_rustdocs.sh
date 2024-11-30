#!/usr/bin/env bash

# Fail on first error, on undefined variables, and on failures in pipelines.
set -euo pipefail

export CARGO_TARGET_DIR=/tmp/test_crates
RUSTDOC_OUTPUT_DIR="$CARGO_TARGET_DIR/doc"
TOPLEVEL="$(git rev-parse --show-toplevel)"
TARGET_DIR="$TOPLEVEL/localdata/test_data"

dir_is_newer_than_file() {
    local dir="$1"
    local file="$2"
    [[ ! -e $file ]] || [[ $(find "$dir" -newer "$file" -exec sh -c 'printf found; kill "$PPID"' \;) ]]
}

# Allow setting an explicit toolchain, like +nightly or +beta.
set +u
# If the first argument starts with +, it's specifying a toolchain,
# and we need to not count it when checking if the user is running
# `regenerate_test_rustdocs.sh crate1 crate2 ...`
case "$1" in
    "+"*)
        TOOLCHAIN="$1"
        shift
        ;;
    *)
        TOOLCHAIN=""
        ;;
esac
set -u
echo "Generating rustdoc with: $(cargo $TOOLCHAIN --version)"
RUSTDOC_CMD="cargo $TOOLCHAIN rustdoc"

# If there are no arguments, run rustdoc on test_crates/*
if [ "$#" -eq 0 ]; then
    CRATES="$(find "$TOPLEVEL/test_crates/" -maxdepth 1 -mindepth 1 -type d)"
    ALWAYS_UPDATE=
# If there are arguments, just regenerate test_crates/$arg for each argument
else
    CRATES="$@"
    ALWAYS_UPDATE=1
fi

for crate_path in $CRATES; do
    # Removing path prefix, leaving only the directory name without forward slashes
    crate=${crate_path#"$TOPLEVEL/test_crates/"}

    if [[ -f "$TOPLEVEL/test_crates/$crate/Cargo.toml" ]]; then
        target="$TARGET_DIR/$crate/rustdoc.json"
        echo "Generating: $crate"

        if [[ -z "$ALWAYS_UPDATE" ]] && ! dir_is_newer_than_file "$crate_path" "$target"; then
            printf 'No updates needed for %s.\n' "$crate"
            continue
        fi

        pushd "$TOPLEVEL/test_crates/$crate"
        RUSTC_BOOTSTRAP=1 $RUSTDOC_CMD -- -Zunstable-options --document-private-items --document-hidden-items --output-format=json
        mkdir -p "$TARGET_DIR/$crate"
        mv "$RUSTDOC_OUTPUT_DIR/$crate.json" "$target"
        popd
    fi
done

unset CARGO_TARGET_DIR
