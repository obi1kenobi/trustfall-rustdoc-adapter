# Contributing

- [Making your first contribution](#making-your-first-contribution)
- [Running `cargo test` for the first time](#running-cargo-test-for-the-first-time)
- [Extending the adapter](#extending-the-adapter)
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

## Extending the adapter

First, identify which information you want to access (whether a function has a body, if a trait is sealed, etc.)

Modify the `src/rustdoc_schema.graphql` file to declare the new information you will be exposing.

Crawl `src/adapter/mod.rs` to find the part that handle the query fragment you modified.

- If the information is trivial, you can simply expose it from the `rustdoc` types using the `field_property!()` macro.
- Sometimes, this information is not core to rustdoc types (e.g. sealed types). You can implement your own logic into `resolve_property_with`.

Create a test crate in `test_crates` with `cargo new <test_crate_name> --lib` and add query tests in `src/adapter/tests.rs`.

Congrats on extending the adapter!

## Development Environment

While `trustfall-rustdoc-adapter` is cross platform, the development task automation scripts in the scripts
directory require a `bash` shell to run.

Windows users can get a bash + GNU command line environment via WSL or git bash.
Linux and macOS typically have bash installed by default.
