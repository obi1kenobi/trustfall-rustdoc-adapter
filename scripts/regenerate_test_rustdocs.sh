#!/usr/bin/env bash

# Fail on first error, on undefined variables, and on failures in pipelines.
set -euo pipefail

BASE_CARGO_TARGET_DIR=/tmp/test_crates
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

# If a toolchain was explicitly requested, always regenerate.
if [[ -n "$TOOLCHAIN" ]]; then
    ALWAYS_UPDATE=1
fi

JOBS=${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)}

CRATES_TO_BUILD=()
for crate_path in $CRATES; do
    crate=${crate_path#"$TOPLEVEL/test_crates/"}
    if [[ -f "$TOPLEVEL/test_crates/$crate/Cargo.toml" ]]; then
        target="$TARGET_DIR/$crate/rustdoc.json"
        if [[ -z "$ALWAYS_UPDATE" ]] && ! dir_is_newer_than_file "$crate_path" "$target"; then
            printf 'No updates needed for %s.\n' "$crate"
            continue
        fi
        CRATES_TO_BUILD+=("$crate")
    fi
done

generate() {
    set -euo pipefail

    local crate="$1"
    local crate_target_dir="$BASE_CARGO_TARGET_DIR/$crate"
    local target="$TARGET_DIR/$crate/rustdoc.json"

    echo "Generating: $crate"
    (
        cd "$TOPLEVEL/test_crates/$crate"
        RUSTC_BOOTSTRAP=1 CARGO_TARGET_DIR="$crate_target_dir" $RUSTDOC_CMD -- -Zunstable-options --document-private-items --document-hidden-items --output-format=json
    )
    mkdir -p "$TARGET_DIR/$crate"
    mv "$crate_target_dir/doc/$crate.json" "$target"
}
export -f generate
export RUSTDOC_CMD TARGET_DIR TOPLEVEL BASE_CARGO_TARGET_DIR

if ((${#CRATES_TO_BUILD[@]})); then
    printf '%s\n' "${CRATES_TO_BUILD[@]}" | xargs --no-run-if-empty -I{} -P "$JOBS" bash -c 'generate "$@"' _ {}
fi
