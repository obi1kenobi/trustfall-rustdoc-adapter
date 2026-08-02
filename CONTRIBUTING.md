# Contributing

- [Making your first contribution](#making-your-first-contribution)
- [Running `cargo test` for the first time](#running-cargo-test-for-the-first-time)
- [Extending the adapter](#extending-the-adapter)
- [Before submitting a pull request](#before-submitting-a-pull-request)
- [Development Environment](#development-environment)

## Making your first contribution

Thanks for taking the time to contribute!

[Here is a list](https://github.com/obi1kenobi/trustfall-rustdoc-adapter/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22+)
of ways to extend the adapter that have all their prerequisites met and are ready to be added,
and which have mentorship available.
Please make use of the mentorship opportunity by asking questions in the relevant GitHub issue!

Make sure to check the ["Development Environment"](#development-environment) section, especially if you are using Windows.

The ["Extending the adapter"](#extending-the-adapter) section of this document has a walkthrough for
defining and testing new query fragments.

Please see the ["Running `cargo test` for the first time"](#running-cargo-test-for-the-first-time)
section to generate the test rustdoc JSON data the tests require. Failing to run this step
will cause `cargo test` failures.

`trustfall-rustdoc-adapter` uses the [Trustfall](https://github.com/obi1kenobi/trustfall) query engine,
which in turn uses GraphQL syntax with non-standard semantics.
These extensions were originally developed for a previous project ("GraphQL compiler"),
and have been streamlined and further developed in Trustfall.
Trustfall documentation is unfortunately still minimal and still consists largely of examples,
but most Trustfall query functionality is nearly identical
(down to trivial parameter naming differences) to the query functionality documented in
[the GraphQL compiler query reference](https://graphql-compiler.readthedocs.io/en/latest/language_specification/query_directives.html).

## Running `cargo test` for the first time

Testing this crate requires rustdoc JSON output data, which is too large and variable
to check into git. It has to be generated locally before `cargo test` will succeed,
and will be saved in a `localdata` gitignored directory in the repo root.

To generate this data, please run `./scripts/regenerate_test_rustdocs.sh`.
To use a specific toolchain, like beta or nightly, pass it as
an argument: `./scripts/regenerate_test_rustdocs.sh +nightly`.

Run this script again whenever you add or modify a test crate. You may pass
one or more test-crate names to regenerate only those crates.

## Extending the adapter

First, identify which information you want to access (whether a function has a body, if a trait is sealed, etc.)

Modify the `src/rustdoc_schema.graphql` file to declare the new information you will be exposing.

Crawl `src/adapter/mod.rs` to find the part that handle the query fragment you modified.

- Properties

  - If the information is trivial, you can simply expose it from the `rustdoc` types using the `field_property!()` macro.

  - Sometimes, this information is not core to rustdoc types (e.g. sealed types). You can implement your own logic into `resolve_property_with`.

- Edges

  TODO

- Vertex

  TODO

Expose the relevant Rust construct in a crate under `test_crates`,
then add query tests in `src/adapter/tests.rs`.

Tests should query rustdoc-generated data through Trustfall rather than
constructing `rustdoc_types` values by hand.

When adding a test crate:

- Set `publish = false` in its `Cargo.toml`.
- Keep it free of unrelated warnings and unused code.
- Check it with `cargo check --manifest-path test_crates/<name>/Cargo.toml`.

Congrats on extending the adapter!

## Before submitting a pull request

Run the basic checks used by CI:

```console
cargo fmt --check
cargo clippy --all-targets --no-deps -- -D warnings --allow deprecated
RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --document-private-items
cargo test
```

Then, as you get ready to open the pull request, note that the GitHub screen for opening PRs
shows your code changes that will be part of that PR.
Please self-review your changes in that UI.
Lots of small bugs that are missed when working in your editor are easily found
through a "change of scenery" to a different UI.
Doing this self-review is very important, and will make sure your PR is more likely to merge quickly
because it means you'll catch and fix bugs that would otherwise have required a slow back-and-forth
with a reviewer.

## Development Environment

While `trustfall-rustdoc-adapter` is cross platform, the development task automation scripts in the scripts
directory require a `bash` shell to run.

Windows users can get a bash + GNU command line environment via WSL or git bash.
Linux and macOS typically have bash installed by default.
