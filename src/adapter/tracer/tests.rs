use std::{
    cell::RefCell, collections::BTreeMap, fmt::Write, path::PathBuf, rc::Rc, sync::Arc,
    time::Duration,
};

use super::ptrace::{Tracer, TracingAdapter};
use crate::{
    RustdocAdapter,
    adapter::tracer::ptrace::{TraceOpType, YieldValue, make_iter_with_perf_span},
};
use anyhow::Context;
use trustfall::{Schema, TryIntoStruct};

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

fn format_operation(op: &TraceOpType) -> String {
    match op {
        TraceOpType::Call(x) => format!("Call({:?})", x),
        TraceOpType::AdvanceInputIterator => format!("AdvanceInputIterator"),
        TraceOpType::YieldInto => format!("YieldInto"),
        TraceOpType::YieldFrom(val) => {
            let x = match val {
                YieldValue::ResolveStartingVertices => format!("ResolveStartingVertices"),
                YieldValue::ResolveProperty => format!("ResolveProperty"),
                YieldValue::ResolveNeighborsOuter => format!("ResolveNeighborsOuter"),
                YieldValue::ResolveNeighborsInner => format!("ResolveNeighborsInner"),
                YieldValue::ResolveCoercion => format!("ResolveCoercion"),
            };
            format!("YieldFrom({})", x)
        }
        TraceOpType::InputIteratorExhausted => format!("InputIteratorExhausted"),
        TraceOpType::OutputIteratorExhausted => format!("OutputIteratorExhausted"),
        TraceOpType::ProduceQueryResult => format!("ProduceQueryResult"),
    }
}

fn trace_to_text(trace: &Tracer) -> String {
    let mut buffer = String::with_capacity(1_000_000);
    for op in &trace.operations() {
        write!(
            &mut buffer,
            "{:?} {:?} {:?} {}\n",
            op.opid,
            op.parent_opid,
            op.duration,
            format_operation(&op.content)
        )
        .unwrap();
    }
    buffer
}

#[test]
fn trace_function_abi() {
    get_test_data!(data, function_abi);
    let adapter = RustdocAdapter::new(&data, None);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                abi_: abi {
                    name @output
                    raw_name @output
                    unwind @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema = Schema::parse(include_str!("../../rustdoc_schema.graphql"))
        .expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        abi_name: String,
        abi_raw_name: String,
        abi_unwind: Option<bool>,
    }
    let tracer = Rc::new(RefCell::new(Tracer::new()));
    let mut tracing_adapter = Arc::new(TracingAdapter::new(&adapter, tracer));

    let tracing_adapter_ref = tracing_adapter.clone();

    let mut results: Vec<_> = make_iter_with_perf_span(
        trustfall::execute_query(&schema, tracing_adapter.clone(), query, variables.clone())
            .expect("failed to run query"),
        move |result, d| {
            tracing_adapter_ref.tracer.borrow_mut().record(
                TraceOpType::ProduceQueryResult,
                None,
                Some(d),
            );
            result
        },
    )
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();

    let trace = Arc::make_mut(&mut tracing_adapter).clone().finish();

    let out_path = PathBuf::from("./test_1.ptrace.txt");
    let buffer = trace_to_text(&trace);
    std::fs::write(out_path, buffer).unwrap();

    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                name: "example_not_unwind".into(),
                abi_name: "C".into(),
                abi_raw_name: "C".into(),
                abi_unwind: Some(false),
            },
            Output {
                name: "example_unwind".into(),
                abi_name: "C".into(),
                abi_raw_name: "C-unwind".into(),
                abi_unwind: Some(true),
            },
            Output {
                name: "rust_abi".into(),
                abi_name: "Rust".into(),
                abi_raw_name: "Rust".into(),
                abi_unwind: Some(true),
            },
        ],
        results
    );
}
