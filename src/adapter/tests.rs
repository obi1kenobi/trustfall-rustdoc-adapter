use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use maplit::btreemap;
use trustfall::{FieldValue, Schema};

use crate::{IndexedCrate, RustdocAdapter};

#[test]
fn rustdoc_json_format_version() {
    let path = "./localdata/test_data/reexport/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let expected_version = rustdoc_types::FORMAT_VERSION;
    let actual_version = crate::test_util::detect_rustdoc_format_version(&content)
        .expect("unrecognized rustdoc format");

    assert_eq!(
        expected_version, actual_version,
        "Expected to find rustdoc v{expected_version} but got v{actual_version} instead.",
    );
}

/// Ensure that methods implemented on references (like `&Foo`) show up in queries.
#[test]
fn impl_for_ref() {
    let path = "./localdata/test_data/impl_for_ref/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = RustdocAdapter::new(&indexed_crate, None);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @filter(op: "=", value: ["$struct"])

                impl @fold @transform(op: "count") @output(name: "matching_methods") {
                    method {
                        name @filter(op: "=", value: ["$method"])
                    }
                }
            }
        }
    }
}
"#;
    let variables = btreemap! {
        "struct" => "StringHolder",
        "method" => "eq",
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");
    let results: Vec<_> = trustfall::execute_query(&schema, Arc::new(adapter), query, variables)
        .expect("failed to run query")
        .collect();

    assert_eq!(
        vec![btreemap! {
            Arc::from("matching_methods") => FieldValue::Uint64(3),
        }],
        results
    );
}

#[test]
fn rustdoc_finds_supertrait() {
    let path = "./localdata/test_data/supertrait/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = RustdocAdapter::new(&indexed_crate, None);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                supertrait {
                    name @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");
    let results: Vec<_> = trustfall::execute_query(&schema, Arc::new(adapter), query, variables)
        .expect("failed to run query")
        .collect();

    assert_eq!(
        vec![
            btreemap! {
                Arc::from("name") => FieldValue::String("Supertrait2".to_string()),
            },
            btreemap! {
                Arc::from("name") => FieldValue::String("Supertrait".to_string()),
            }
        ],
        results
    );
}
