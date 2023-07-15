use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use maplit::btreemap;
use trustfall::{FieldValue, Schema, TryIntoStruct};

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

#[test]
fn rustdoc_finds_consts() {
    let path = "./localdata/test_data/consts/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let query = r#"
{
    Crate {
        item {
            ... on Constant {
                name @output

                importable_path {
                    path @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        path: Vec<String>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    assert_eq!(
        vec![
            Output {
                name: "FIRST".into(),
                path: vec!["consts".into(), "FIRST".into()],
            },
            Output {
                name: "SECOND".into(),
                path: vec!["consts".into(), "inner".into(), "SECOND".into()],
            },
        ],
        results
    );

    // Ensure that querying for GlobalValue items also retrieves all consts.
    let global_values_query = r#"
{
    Crate {
        item {
            ... on GlobalValue {
                name @output

                importable_path {
                    path @output
                }
            }
        }
    }
}
"#;
    let mut global_values_results: Vec<_> =
        trustfall::execute_query(&schema, adapter, global_values_query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    global_values_results.sort_unstable();
    assert_eq!(results, global_values_results);
}

#[test]
fn rustdoc_trait_has_associated_types() {
    let path = "./localdata/test_data/traits_with_associated_types/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                associated_type {
                    name @output
                    has_default @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        has_default: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    assert_eq!(
        vec![
            Output {
                name: "DeserializedType".into(),
                has_default: false,
            },
            Output {
                name: "SerializedType".into(),
                has_default: true,
            },
        ],
        results
    );
}

#[test]
fn rustdoc_finds_statics() {
    let path = "./localdata/test_data/statics/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let query = r#"
{
    Crate {
        item {
            ... on Static {
                name @output
                mutable @output

                importable_path {
                    path @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        path: Vec<String>,
        mutable: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    assert_eq!(
        vec![
            Output {
                name: "FIRST".into(),
                path: vec!["statics".into(), "FIRST".into()],
                mutable: false,
            },
            Output {
                name: "MUT".into(),
                path: vec!["statics".into(), "MUT".into()],
                mutable: true,
            },
            Output {
                name: "SECOND".into(),
                path: vec!["statics".into(), "inner".into(), "SECOND".into()],
                mutable: false,
            },
        ],
        results
    );

    // Ensure that querying for GlobalValue items also retrieves all statics.
    let global_values_query = r#"
{
    Crate {
        item {
            ... on GlobalValue {
                name @output

                importable_path {
                    path @output
                }
            }
        }
    }
}
"#;
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct OutputWithoutMut {
        name: String,
        path: Vec<String>,
    }

    let mut global_values_results: Vec<OutputWithoutMut> =
        trustfall::execute_query(&schema, adapter, global_values_query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    global_values_results.sort_unstable();
    assert_eq!(results.len(), global_values_results.len());
    for (expected, actual) in results.into_iter().zip(global_values_results) {
        assert_eq!(expected.name, actual.name);
        assert_eq!(expected.path, actual.path);
    }
}

#[test]
fn rustdoc_associated_consts() {
    let path = "./localdata/test_data/associated_consts/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let impl_owner_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                inherent_impl {
                    associated_constant {
                        name @output
                        default @output
                    }
                }
            }
        }
    }
}
"#;
    let trait_query = r#"
{
    Crate {
        item {
            ... on Trait {
                associated_constant {
                    name @output
                    default @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        default: Option<String>,
    }

    let mut results: Vec<_> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        impl_owner_query,
        variables.clone(),
    )
    .expect("failed to run query")
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();
    results.sort_unstable();

    assert_eq!(
        vec![Output {
            name: "START".into(),
            default: Some("0".into()),
        },],
        results
    );

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), trait_query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    assert_eq!(
        vec![
            Output {
                name: "DEFAULT_BATCH_SIZE".into(),
                default: Some("16".into()),
            },
            Output {
                name: "INVALID_BATCH_SIZE".into(),
                default: Some("_".into()), // evaluating a const expression
            },
            Output {
                name: "LOG_AS".into(),
                default: Some("\"[batch]\"".into()),
            },
            Output {
                name: "MAX_BATCH_SIZE".into(),
                default: None,
            },
            Output {
                name: "MIN_BATCH_SIZE".into(),
                default: Some("_".into()), // call to a `const fn`
            },
        ],
        results
    );
}
