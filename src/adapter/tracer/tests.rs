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
    for op in &trace.calls {
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
