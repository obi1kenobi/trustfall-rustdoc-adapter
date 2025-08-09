use std::{cell::RefCell, collections::BTreeMap, fmt::Write, path::PathBuf, rc::Rc, sync::Arc};

use super::ptrace::{Tracer, TracingAdapter};
use crate::{
    RustdocAdapter,
    adapter::tracer::ptrace::{TraceOpType, YieldValue, trace_results},
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
    for op in &trace.ops {
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

    let _results: Vec<Output> = trace_results(
        tracing_adapter.clone(),
        trustfall::execute_query(&schema, tracing_adapter.clone(), query, variables.clone())
            .expect("failed to run query"),
    )
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();

    let trace = Arc::make_mut(&mut tracing_adapter).clone().finish();

    let out_path = PathBuf::from("./test_1.ptrace.txt");
    let buffer = trace_to_text(&trace);
    std::fs::write(out_path, buffer).unwrap();
}
