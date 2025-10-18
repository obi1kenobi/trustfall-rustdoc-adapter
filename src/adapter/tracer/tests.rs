// The Trustfall API requires the adapter to be passed in as an Arc.
// Our adapter is not Send/Sync (it doesn't need it),
// but there's currently nothing we can do about this lint.
#![expect(clippy::arc_with_non_send_sync)]

use std::{collections::BTreeMap, num::NonZero, sync::Arc, time::Duration};

use anyhow::Context;
use trustfall::Schema;
use trustfall::provider::{Eid, Vid};

use super::ptrace::{ExpHistogram, FunctionCall, Summary, TracingAdapter};
use crate::RustdocAdapter;

// Copied from src/adapter/tests.rs
macro_rules! get_test_data {
    ($data:ident, $case:ident) => {
        let rustdoc_path = format!("./localdata/test_data/{}/rustdoc.json", stringify!($case));
        let content = std::fs::read_to_string(&rustdoc_path)
            .with_context(|| format!("Could not load {rustdoc_path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to load rustdoc");
        let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");

        let manifest_path = format!("./test_crates/{}/Cargo.toml", stringify!($case));

        let mut metadata = cargo_metadata::MetadataCommand::new().manifest_path(&manifest_path).no_deps().exec().expect("failed to run cargo metadata");
        assert_eq!(metadata.packages.len(), 1, "{metadata:?}");
        let package = metadata.packages.pop().expect("failed to pop only item in vec");

        let storage = crate::PackageStorage::from_rustdoc_and_package(
            crate_,
            package,
        );

        let $data = crate::PackageIndex::from_storage(&storage);
    }
}

#[test]
fn exp_histogram() {
    let mut hist = ExpHistogram::new();
    assert_eq!(hist.count(), 0);
    assert!(hist.buckets().iter().all(|&b| b == 0));

    assert_eq!(
        hist.boundaries(),
        &[
            100,
            300,
            1000,
            3000,
            10000,
            30000,
            100000,
            300000,
            1000000,
            3000000,
            10000000,
            30000000,
            100000000,
            300000000,
            1000000000,
            u64::MAX
        ],
    );

    // Test adding entries at the top and bottom of the boundaries.
    for (i, upper_lim) in hist.boundaries()[..15].iter().enumerate() {
        hist.add(*upper_lim);
        hist.add(*upper_lim + 1);
        hist.add(*upper_lim - 1);

        if i == 0 {
            assert_eq!(hist.buckets()[i], 2);
        } else {
            assert_eq!(hist.buckets()[i], 3);
        }

        assert_eq!(hist.count() as usize, 3 * (i + 1));
    }

    hist.add(u64::MAX);
    hist.add(10u64.pow(17));
    assert_eq!(hist.buckets()[15], 3);
}

#[test]
fn summary() {
    let dur = Duration::from_nanos(100);
    let mut summary = Summary::new(dur);

    assert_eq!(summary.count(), 1);
    assert_eq!(summary.histogram().count(), 1);
    assert_eq!(summary.min(), dur);
    assert_eq!(summary.max(), dur);
    assert_eq!(summary.total(), dur);
    assert_eq!(summary.mean(), dur);

    summary.update(Duration::from_nanos(50));
    summary.update(Duration::from_nanos(200));

    assert_eq!(summary.count(), 3);
    assert_eq!(summary.histogram().count(), 3);
    assert_eq!(summary.min(), Duration::from_nanos(50));
    assert_eq!(summary.max(), Duration::from_nanos(200));
    assert_eq!(summary.total(), Duration::from_nanos(350));
    assert_eq!(summary.mean(), Duration::from_nanos(350 / 3));
}

#[test]
fn tracing_adapter() {
    // Confirm that the trace is the same.
    // If this test fails, but the adapter tests all pass, then it's likely
    // that this is being affected by a functionality change in the adapter.
    // In that case, replace the `desired` list with the output of `tracer.calls`.
    get_test_data!(data, function_has_body);
    let adapter = RustdocAdapter::new(&data, None);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                owner: name @output

                method {
                    name @output
                    has_body @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema = Schema::parse(include_str!("../../rustdoc_schema.graphql"))
        .expect("schema failed to parse");

    let tracing_adapter = Arc::new(TracingAdapter::new(&adapter));

    let mut _results: Vec<_> =
        trustfall::execute_query(&schema, tracing_adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .collect();

    let tracer = tracing_adapter.finish();

    // List of (function call, count) tuples.
    let desired = [
        (
            FunctionCall::ResolveProperty(
                Vid::new(NonZero::new(2).unwrap()),
                "Trait".into(),
                "name".into(),
            ),
            2,
        ),
        (
            FunctionCall::ResolveProperty(
                Vid::new(NonZero::new(3).unwrap()),
                "Method".into(),
                "has_body".into(),
            ),
            2,
        ),
        (
            FunctionCall::ResolveProperty(
                Vid::new(NonZero::new(3).unwrap()),
                "Method".into(),
                "name".into(),
            ),
            2,
        ),
        (
            FunctionCall::ResolveNeighbors(
                Vid::new(NonZero::new(1).unwrap()),
                "Crate".into(),
                Eid::new(NonZero::new(1).unwrap()),
            ),
            1,
        ),
        (
            FunctionCall::ResolveNeighbors(
                Vid::new(NonZero::new(2).unwrap()),
                "Trait".into(),
                Eid::new(NonZero::new(2).unwrap()),
            ),
            1,
        ),
        (
            FunctionCall::ResolveNeighborsInner(
                Vid::new(NonZero::new(1).unwrap()),
                "Crate".into(),
                Eid::new(NonZero::new(1).unwrap()),
            ),
            20,
        ),
        (
            FunctionCall::ResolveNeighborsInner(
                Vid::new(NonZero::new(2).unwrap()),
                "Trait".into(),
                Eid::new(NonZero::new(2).unwrap()),
            ),
            2,
        ),
        (
            FunctionCall::ResolveCoercion(
                Vid::new(NonZero::new(2).unwrap()),
                "Item".into(),
                "Trait".into(),
            ),
            20,
        ),
    ];

    for (i, (call, summary)) in tracer.calls.iter().enumerate() {
        assert_eq!(*call, desired[i].0);
        assert_eq!(summary.count(), desired[i].1);
    }
}
