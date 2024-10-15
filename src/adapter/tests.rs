// The Trustfall API requires the adapter to be passed in as an Arc.
// Our adapter is not Send/Sync (it doesn't need it),
// but there's currently nothing we can do about this lint.
#![allow(clippy::arc_with_non_send_sync)]

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use maplit::btreemap;
use trustfall::{Schema, TryIntoStruct};

use crate::{IndexedCrate, RustdocAdapter};

#[allow(dead_code)]
mod type_level_invariants {
    use crate::{IndexedCrate, RustdocAdapter};

    fn ensure_send_and_sync<T: Send + Sync>(_value: &T) {}

    fn ensure_indexed_crate_is_sync(value: &IndexedCrate<'_>) {
        ensure_send_and_sync(value);
    }

    fn ensure_adapter_is_sync(value: &RustdocAdapter<'_>) {
        ensure_send_and_sync(value);
    }
}

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

#[test]
fn adapter_invariants() {
    // Which rustdoc file we use doesn't really matter,
    // we just need it to create the `RustdocAdapter` struct.
    let path = "./localdata/test_data/impl_for_ref/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = RustdocAdapter::new(&indexed_crate, None);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    trustfall::provider::check_adapter_invariants(&schema, adapter)
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

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        matching_methods: u64,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![Output {
            matching_methods: 3
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
                name @output

                supertrait {
                    supertrait: name @output
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
        supertrait: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                name: "DebugPartialOrd".into(),
                // We *specifically* require the supertrait name to be "Debug",
                // not "std::fmt::Debug" or any other option. Failing to do this
                // could cause false-positives in cargo-semver-checks.
                supertrait: "Debug".into(),
            },
            Output {
                name: "DebugPartialOrd".into(),
                supertrait: "PartialOrd".into(),
            },
            Output {
                name: "MyTrait".into(),
                supertrait: "Supertrait".into(),
            },
            Output {
                name: "MyTrait".into(),
                supertrait: "Supertrait2".into(),
            },
        ],
        results
    );
}

#[test]
fn rustdoc_sealed_traits() {
    let path = "./localdata/test_data/sealed_traits/rustdoc.json";
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
                name @output
                sealed @output
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
        sealed: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "Sealed".into(),
            sealed: true,
        },
        Output {
            name: "InternalMarker".into(),
            sealed: true,
        },
        Output {
            name: "DirectlyTraitSealed".into(),
            sealed: true,
        },
        Output {
            name: "TransitivelyTraitSealed".into(),
            sealed: true,
        },
        Output {
            name: "SealedTraitWithStdSupertrait".into(),
            sealed: true,
        },
        Output {
            name: "SealedWithWhereSelfBound".into(),
            sealed: true,
        },
        Output {
            name: "PrivateSealed".into(),
            sealed: true,
        },
        Output {
            name: "SealedWithPrivateSupertrait".into(),
            sealed: true,
        },
        Output {
            name: "Unsealed".into(),
            sealed: false,
        },
        Output {
            name: "MethodSealed".into(),
            sealed: true,
        },
        Output {
            name: "TransitivelyMethodSealed".into(),
            sealed: true,
        },
        Output {
            name: "NotMethodSealedBecauseOfDefaultImpl".into(),
            sealed: false,
        },
        Output {
            name: "NotTransitivelySealed".into(),
            sealed: false,
        },
        Output {
            name: "TraitUnsealedButMethodGenericSealed".into(),
            sealed: false,
        },
        Output {
            name: "NotGenericSealedBecauseOfDefaultImpl".into(),
            sealed: false,
        },
        Output {
            name: "IteratorExt".into(),
            sealed: false,
        },
        Output {
            name: "Iterator".into(),
            sealed: true,
        },
        Output {
            name: "ShadowedSubIterator".into(),
            sealed: true,
        },
        Output {
            name: "Super".into(),
            sealed: false,
        },
        Output {
            name: "Marker".into(),
            sealed: true,
        },
        Output {
            name: "NotGenericSealedBecauseOfPubSupertrait".into(),
            sealed: false,
        },
        Output {
            name: "FullBlanket".into(),
            sealed: true,
        },
        Output {
            name: "PrivateBlanket".into(),
            sealed: true,
        },
        Output {
            name: "RefBlanket".into(),
            sealed: true,
        },
        Output {
            name: "ExternalSupertraitsBlanket".into(),
            sealed: true,
        },
        Output {
            name: "BlanketWithWhereClause".into(),
            sealed: true,
        },
        Output {
            name: "IteratorBlanket".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverLocalUnsealedTrait".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverSealedTrait".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverSealedAndUnsealedTrait".into(),
            sealed: true,
        },
        Output {
            name: "TransitiveBlanket".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverArc".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverTuple".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverSlice".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverArray".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverPointer".into(),
            sealed: true,
        },
        Output {
            name: "BlanketUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "RefBlanketUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "ExternalSupertraitsBlanketUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "BlanketWithWhereClauseUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "IteratorBlanketUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "BlanketOverLocalUnsealedTraitUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "BlanketOverSealedTraitSealed".into(),
            sealed: true,
        },
        Output {
            name: "BlanketSealedOverMultiple".into(),
            sealed: true,
        },
        Output {
            name: "TransitiveBlanketUnsealed".into(),
            sealed: false,
        },
        Output {
            name: "BlanketOverArcSealed".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverTupleSealed".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverSliceSealed".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverArraySealed".into(),
            sealed: true,
        },
        Output {
            name: "BlanketOverPointerSealed".into(),
            sealed: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
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
                expr @output
                value @output
                is_literal @output

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
        expr: String,
        value: Option<String>,
        is_literal: bool,
    }
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct OutputSimple {
        name: String,
        path: Vec<String>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct::<Output>().expect("shape mismatch"))
            .collect();
    results.sort_unstable();
    // to compare to GlobalValue that doesn't Constant-specific properties
    let mut results_simple: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| {
                row.try_into_struct::<OutputSimple>()
                    .expect("shape mismatch")
            })
            .collect();
    results_simple.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                name: "FIRST".into(),
                path: vec!["consts".into(), "FIRST".into()],
                expr: "1".to_string(),
                value: Some("1u32".to_string()),
                is_literal: true,
            },
            Output {
                name: "SECOND".into(),
                path: vec!["consts".into(), "inner".into(), "SECOND".into()],
                expr: "2".to_string(),
                value: Some("2i64".to_string()),
                is_literal: true,
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
            .map(|row| {
                row.try_into_struct::<OutputSimple>()
                    .expect("shape mismatch")
            })
            .collect();
    global_values_results.sort_unstable();
    assert_eq!(results_simple, global_values_results);
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

    similar_asserts::assert_eq!(
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

    similar_asserts::assert_eq!(
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
fn rustdoc_modules() {
    let path = "./localdata/test_data/modules/rustdoc.json";

    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let mod_query = r#"
{
    Crate {
        item {
            ... on Module {
                module: name @output
                is_stripped @output

                item @fold {
                    members: name @output
                    types: __typename @output
                }

                importable_path @fold {
                    paths: path @output
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
        module: String,
        is_stripped: bool,
        members: Vec<Option<String>>,
        types: Vec<String>,
        paths: Vec<Vec<String>>,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), mod_query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                module: "hello".into(),
                is_stripped: false,
                members: vec![Some("world".into()), Some("T2".into())],
                types: vec!["Module".into(), "Struct".into()],
                paths: vec![
                    vec!["modules".into(), "hello".into()],
                    vec!["modules".into(), "hi".into()]
                ],
            },
            Output {
                module: "inner".into(),
                is_stripped: false,
                members: vec![Some("T4".into(),),],
                types: vec!["Struct".into()],
                paths: vec![],
            },
            Output {
                module: "modules".into(),
                is_stripped: false,
                members: vec![Some("hello".into()), Some("outer".into())],
                types: vec!["Module".into(), "Module".into()],
                paths: vec![vec!["modules".into()]],
            },
            Output {
                module: "outer".into(),
                is_stripped: false,
                members: vec![Some("inner".into()), Some("T3".into())],
                types: vec!["Module".into(), "Struct".into()],
                paths: vec![vec!["modules".into(), "outer".into()]],
            },
            Output {
                module: "world".into(),
                is_stripped: false,
                members: vec![Some("T1".into())],
                types: vec!["Struct".into()],
                paths: vec![
                    vec!["modules".into(), "hello".into(), "world".into()],
                    vec!["modules".into(), "hi".into(), "world".into()],
                ],
            },
        ],
        results
    );

    let root_query = r#"
{
    Crate {
        root_module {
            module: name @output
            is_stripped @output

            item @fold {
                members: name @output
                types: __typename @output
            }

            importable_path @fold {
                paths: path @output
            }
        }
    }
}
"#;

    let results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), root_query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();

    similar_asserts::assert_eq!(
        vec![Output {
            module: "modules".into(),
            is_stripped: false,
            members: vec![Some("hello".into()), Some("outer".into())],
            types: vec!["Module".into(), "Module".into()],
            paths: vec![vec!["modules".into()]]
        }],
        results
    );
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

    similar_asserts::assert_eq!(
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

    similar_asserts::assert_eq!(
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

#[test]
fn function_abi() {
    let path = "./localdata/test_data/function_abi/rustdoc.json";
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

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        abi_name: String,
        abi_raw_name: String,
        abi_unwind: Option<bool>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
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

#[test]
fn function_export_name() {
    let path = "./localdata/test_data/function_export_name/rustdoc.json";
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
            ... on Function {
                name @output
                export_name @output
                visibility_limit @output
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
        export_name: Option<String>,
        visibility_limit: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                name: "example_export_name".into(),
                export_name: Some("renamed".into()),
                visibility_limit: "public".into(),
            },
            Output {
                name: "example_not_mangled".into(),
                export_name: Some("example_not_mangled".into()),
                visibility_limit: "public".into(),
            },
            Output {
                name: "mangled".into(),
                export_name: None,
                visibility_limit: "public".into(),
            },
            Output {
                name: "private_export_name".into(),
                export_name: Some("private_renamed".into()),
                visibility_limit: "crate".into(),
            },
            Output {
                name: "private_not_mangled".into(),
                export_name: Some("private_not_mangled".into()),
                visibility_limit: "crate".into(),
            },
        ],
        results
    );
}

#[test]
fn importable_paths() {
    let path = "./localdata/test_data/importable_paths/rustdoc.json";
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
            ... on Struct {
                name @output
                importable_path {
                    path @output
                    doc_hidden @output
                    deprecated @output
                    public_api @output
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
        doc_hidden: bool,
        deprecated: bool,
        public_api: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "PublicImportable".into(),
            path: vec!["importable_paths".into(), "PublicImportable".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "ModuleHidden".into(),
            path: vec![
                "importable_paths".into(),
                "hidden".into(),
                "ModuleHidden".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "DeprecatedModuleHidden".into(),
            path: vec![
                "importable_paths".into(),
                "hidden".into(),
                "DeprecatedModuleHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedModuleHidden".into(),
            path: vec![
                "importable_paths".into(),
                "hidden".into(),
                "deprecated".into(),
                "ModuleDeprecatedModuleHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "Hidden".into(),
            path: vec![
                "importable_paths".into(),
                "submodule".into(),
                "Hidden".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "DeprecatedHidden".into(),
            path: vec![
                "importable_paths".into(),
                "submodule".into(),
                "DeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec![
                "importable_paths".into(),
                "deprecated".into(),
                "ModuleDeprecated".into(),
            ],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedHidden".into(),
            path: vec![
                "importable_paths".into(),
                "deprecated".into(),
                "ModuleDeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleHidden".into(),
            path: vec!["importable_paths".into(), "UsedVisible".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "Hidden".into(),
            path: vec!["importable_paths".into(), "UsedHidden".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec!["importable_paths".into(), "UsedModuleDeprecated".into()],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedHidden".into(),
            path: vec![
                "importable_paths".into(),
                "UsedModuleDeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "PublicImportable".into(),
            path: vec![
                "importable_paths".into(),
                "reexports".into(),
                "DeprecatedReexport".into(),
            ],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "PublicImportable".into(),
            path: vec![
                "importable_paths".into(),
                "reexports".into(),
                "HiddenReexport".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec![
                "importable_paths".into(),
                "reexports".into(),
                "HiddenDeprecatedReexport".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "Aliased".into(),
            path: vec!["importable_paths".into(), "Aliased".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn item_own_public_api_properties() {
    let path = "./localdata/test_data/importable_paths/rustdoc.json";
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
            ... on Struct {
                name @output
                doc_hidden @output
                deprecated @output
                public_api_eligible @output
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
        doc_hidden: bool,
        deprecated: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We are checking whether the *items themselves* are deprecated / hidden.
    // We are *not* checking whether their paths are deprecated or hidden.
    // Recall that Rust propagates deprecations into child item definitions,
    // but does not propagate "hidden"-ness.
    //
    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "PublicImportable".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "PubInPriv".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "Private".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: false,
        },
        Output {
            name: "ModuleHidden".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "DeprecatedModuleHidden".into(),
            doc_hidden: false,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "ModuleDeprecatedModuleHidden".into(),
            doc_hidden: false,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "Hidden".into(),
            doc_hidden: true,
            deprecated: false,
            public_api_eligible: false,
        },
        Output {
            name: "DeprecatedHidden".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "ModuleDeprecated".into(),
            doc_hidden: false,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "ModuleDeprecatedHidden".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "Aliased".into(),
            doc_hidden: true,
            deprecated: false,
            public_api_eligible: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Enum variants have as-if-public visibility by default -- they are public if the enum is public.
#[test]
fn enum_variant_public_api_eligible() {
    let path = "./localdata/test_data/importable_paths/rustdoc.json";
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
            ... on Variant {
                name @output
                doc_hidden @output
                deprecated @output
                public_api_eligible @output
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
        doc_hidden: bool,
        deprecated: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We are checking whether the *items themselves* are deprecated / hidden.
    // We are *not* checking whether their paths are deprecated or hidden.
    // This is why it doesn't matter that the enum itself is private.
    //
    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "NotHidden".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "Deprecated".into(),
            doc_hidden: false,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "DeprecatedHidden".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true,
        },
        Output {
            name: "Hidden".into(),
            doc_hidden: true,
            deprecated: false,
            public_api_eligible: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Trait associated items have as-if-public visibility by default.
#[test]
fn trait_associated_items_public_api_eligible() {
    let path = "./localdata/test_data/importable_paths/rustdoc.json";
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
                name @filter(op: "=", value: ["$trait"])

                associated_type {
                    name @output
                    doc_hidden @output
                    deprecated @output
                    public_api_eligible @output
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, &str> = btreemap! {
        "trait" => "SomeTrait"
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        doc_hidden: bool,
        deprecated: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![Output {
            name: "T".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true
        },],
        results
    );

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                associated_constant {
                    name @output
                    doc_hidden @output
                    deprecated @output
                    public_api_eligible @output
                }
            }
        }
    }
}
"#;

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![Output {
            name: "N".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true
        },],
        results
    );

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                method {
                    name @output
                    doc_hidden @output
                    deprecated @output
                    public_api_eligible @output
                }
            }
        }
    }
}
"#;

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![Output {
            name: "associated".into(),
            doc_hidden: true,
            deprecated: true,
            public_api_eligible: true
        },],
        results
    );
}

#[test]
fn unions() {
    let path = "./localdata/test_data/unions/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    // Part 1: make sure unions have correct visibility (similart to importable_paths
    // test case)

    let query = r#"
{
    Crate {
        item {
            ... on Union {
                name @output
                importable_path {
                    path @output
                    doc_hidden @output
                    deprecated @output
                    public_api @output
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
        doc_hidden: bool,
        deprecated: bool,
        public_api: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "PublicImportable".into(),
            path: vec!["unions".into(), "PublicImportable".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "ModuleHidden".into(),
            path: vec!["unions".into(), "hidden".into(), "ModuleHidden".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "DeprecatedModuleHidden".into(),
            path: vec![
                "unions".into(),
                "hidden".into(),
                "DeprecatedModuleHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedModuleHidden".into(),
            path: vec![
                "unions".into(),
                "hidden".into(),
                "deprecated".into(),
                "ModuleDeprecatedModuleHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "Hidden".into(),
            path: vec!["unions".into(), "submodule".into(), "Hidden".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "DeprecatedHidden".into(),
            path: vec![
                "unions".into(),
                "submodule".into(),
                "DeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec![
                "unions".into(),
                "deprecated".into(),
                "ModuleDeprecated".into(),
            ],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedHidden".into(),
            path: vec![
                "unions".into(),
                "deprecated".into(),
                "ModuleDeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleHidden".into(),
            path: vec!["unions".into(), "UsedVisible".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "Hidden".into(),
            path: vec!["unions".into(), "UsedHidden".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec!["unions".into(), "UsedModuleDeprecated".into()],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "ModuleDeprecatedHidden".into(),
            path: vec!["unions".into(), "UsedModuleDeprecatedHidden".into()],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "PublicImportable".into(),
            path: vec![
                "unions".into(),
                "reexports".into(),
                "DeprecatedReexport".into(),
            ],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "PublicImportable".into(),
            path: vec!["unions".into(), "reexports".into(), "HiddenReexport".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "ModuleDeprecated".into(),
            path: vec![
                "unions".into(),
                "reexports".into(),
                "HiddenDeprecatedReexport".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    // Part 2: make sure union data is properly queryable

    let query = r#"
{
    Crate {
        item {
            ... on Module {
                name @filter(op: "=", value: ["$data"])

                item {
                    ... on Union {
                        union_name: name @output
                        field @fold {
                            visibility_limit @filter(op: "=", value: ["$public"])
                            name @output
                            raw_type {
                                type_name: name @output
                            }
                        }
                    }
                }
            }
        }
    }
}"#;

    let variables: BTreeMap<&str, &str> = btreemap! { "data" => "data" , "public" => "public"};

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct FieldInfo {
        union_name: String,
        name: Vec<String>,
        type_name: Vec<String>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct::<FieldInfo>().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        FieldInfo {
            union_name: "NoFieldsPublic".into(),
            name: vec![],
            type_name: vec![],
        },
        FieldInfo {
            union_name: "SomeFieldsPublic".into(),
            name: vec!["y".into()],
            type_name: vec!["f32".into()],
        },
        FieldInfo {
            union_name: "AllFieldsPublic".into(),
            name: vec!["x".into(), "y".into()],
            type_name: vec!["usize".into(), "f32".into()],
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_has_body() {
    let path = "./localdata/test_data/function_has_body/rustdoc.json";
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
            ... on Function {
                name @output
                has_body @output
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
        has_body: bool,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "top_level".into(),
            has_body: true,
        },
        Output {
            name: "inside_impl_block".into(),
            has_body: true,
        },
        Output {
            name: "trait_no_body".into(),
            has_body: false,
        },
        Output {
            name: "trait_with_body".into(),
            has_body: true,
        },
        Output {
            name: "extern_no_body".into(),
            has_body: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_discriminants() {
    let path = "./localdata/test_data/enum_discriminants/rustdoc.json";
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
            ... on Enum {
                enum_name: name @output
                variant {
                    variant_name: name @output
                    discriminant @optional {
                        value @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, &str> = btreemap! {};

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        enum_name: String,
        variant_name: String,
        value: Option<String>,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                enum_name: "A".into(),
                variant_name: "Five".into(),
                value: Some("100".into(),),
            },
            Output {
                enum_name: "A".into(),
                variant_name: "Four".into(),
                value: Some("99".into(),),
            },
            Output {
                enum_name: "A".into(),
                variant_name: "One".into(),
                value: Some("1".into(),),
            },
            Output {
                enum_name: "A".into(),
                variant_name: "Three".into(),
                value: Some("3".into(),),
            },
            Output {
                enum_name: "A".into(),
                variant_name: "Two".into(),
                value: Some("2".into(),),
            },
            Output {
                enum_name: "A".into(),
                variant_name: "Zero".into(),
                value: Some("0".into(),),
            },
            Output {
                enum_name: "Fieldful".into(),
                variant_name: "Struct".into(),
                value: Some("2".into(),),
            },
            Output {
                enum_name: "Fieldful".into(),
                variant_name: "Tuple".into(),
                value: Some("1".into(),),
            },
            Output {
                enum_name: "Fieldful".into(),
                variant_name: "Unit".into(),
                value: Some("0".into(),),
            },
            Output {
                enum_name: "Fieldful".into(),
                variant_name: "Unit2".into(),
                value: Some("9".into(),),
            },
            Output {
                enum_name: "FieldfulNoRepr".into(),
                variant_name: "Struct".into(),
                value: None,
            },
            Output {
                enum_name: "FieldfulNoRepr".into(),
                variant_name: "Tuple".into(),
                value: None,
            },
            Output {
                enum_name: "FieldfulNoRepr".into(),
                variant_name: "Unit".into(),
                value: None,
            },
            Output {
                enum_name: "FieldlessWithDiscrimants".into(),
                variant_name: "First".into(),
                value: Some("10".into(),),
            },
            Output {
                enum_name: "FieldlessWithDiscrimants".into(),
                variant_name: "Second".into(),
                value: Some("20".into(),),
            },
            Output {
                enum_name: "FieldlessWithDiscrimants".into(),
                variant_name: "Struct".into(),
                value: Some("21".into(),),
            },
            Output {
                enum_name: "FieldlessWithDiscrimants".into(),
                variant_name: "Tuple".into(),
                value: Some("11".into(),),
            },
            Output {
                enum_name: "FieldlessWithDiscrimants".into(),
                variant_name: "Unit".into(),
                value: Some("22".into(),),
            },
            Output {
                enum_name: "Pathological".into(),
                variant_name: "Max".into(),
                value: Some("170141183460469231731687303715884105727".into(),),
            },
            Output {
                enum_name: "Pathological".into(),
                variant_name: "Min".into(),
                value: Some("-170141183460469231731687303715884105728".into(),),
            },
            Output {
                enum_name: "Pathological".into(),
                variant_name: "MinPlusOne".into(),
                value: Some("-170141183460469231731687303715884105727".into(),),
            },
            Output {
                enum_name: "Pathological".into(),
                variant_name: "MinPlusTwo".into(),
                value: Some("-170141183460469231731687303715884105726".into(),),
            },
        ],
        results
    );
}

#[test]
fn declarative_macros() {
    let path = "./localdata/test_data/declarative_macros/rustdoc.json";
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
            ... on Macro {
                name @output
                public_api_eligible @output
                visibility_limit @output

                attribute @optional {
                    raw_attribute @output
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
        public_api_eligible: bool,
        visibility_limit: String,
        raw_attribute: Option<String>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "top_level".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[macro_export]".into()),
        },
        Output {
            name: "nested_private".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[macro_export]".into()),
        },
        Output {
            name: "nested_public".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[macro_export]".into()),
        },
        Output {
            name: "not_exported".into(),
            public_api_eligible: false,
            visibility_limit: "crate".into(),
            raw_attribute: Some("#[allow(unused_macros)]".into()),
        },
        Output {
            name: "hidden_parent".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[macro_export]".into()),
        },
        Output {
            name: "hidden".into(),
            public_api_eligible: false,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[doc(hidden)]".into()),
        },
        Output {
            name: "hidden".into(),
            public_api_eligible: false,
            visibility_limit: "public".into(),
            raw_attribute: Some("#[macro_export]".into()),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn proc_macros() {
    let path = "./localdata/test_data/proc_macros/rustdoc.json";
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
            ... on ProcMacro {
                kind: __typename @output
                name @output
                public_api_eligible @output
                visibility_limit @output
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
        kind: String,
        name: String,
        public_api_eligible: bool,
        visibility_limit: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            kind: "FunctionLikeProcMacro".into(),
            name: "make_answer".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
        },
        Output {
            kind: "AttributeProcMacro".into(),
            name: "return_as_is".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
        },
        Output {
            kind: "DeriveProcMacro".into(),
            name: "AnswerFn".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
        },
        Output {
            kind: "DeriveProcMacro".into(),
            name: "HelperAttr".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
        },
        Output {
            kind: "FunctionLikeProcMacro".into(),
            name: "hidden".into(),
            public_api_eligible: false,
            visibility_limit: "public".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    // Ensure that derive macro helper attributes can be queried correctly.
    let query = r#"
{
    Crate {
        item {
            ... on DeriveProcMacro {
                name @output

                helper_attribute {
                    attr: name @output
                }
            }
        }
    }
}
"#;

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct DeriveOutput {
        name: String,
        attr: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        DeriveOutput {
            name: "HelperAttr".into(),
            attr: "helper".into(),
        },
        DeriveOutput {
            name: "HelperAttr".into(),
            attr: "second".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn generic_parameters() {
    let path = "./localdata/test_data/generic_parameters/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let top_level_query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                name @output

                # TODO: HACK, remove this -- workaround for issue:
                # https://github.com/obi1kenobi/trustfall-rustdoc-adapter/issues/400
                #
                # This clause ensures this query doesn't return methods while #400 isn't resolved.
                name @filter(op: "!=", value: ["$method_name"])

                generic_parameter {
                    generic_kind: __typename @output
                    generic_name: name @output
                }
            }
        }
    }
}
"#;
    let impl_owner_methods_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                impl {
                    method {
                        name @output

                        generic_parameter {
                            generic_kind: __typename @output
                            generic_name: name @output
                        }
                    }
                }
            }
        }
    }
}
"#;
    let trait_methods_query = r#"
{
    Crate {
        item {
            ... on Trait {
                method {
                    name @output

                    generic_parameter {
                        generic_kind: __typename @output
                        generic_name: name @output
                    }
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, i64> = BTreeMap::default();
    let mut top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();
    top_level_variables.insert("method_name", "method");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        generic_kind: String,
        generic_name: String,
    }

    let mut results: Vec<_> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        top_level_query,
        top_level_variables.clone(),
    )
    .expect("failed to run top level query")
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            impl_owner_methods_query,
            variables.clone(),
        )
        .expect("failed to run impl owners query"),
    )
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            trait_methods_query,
            variables.clone(),
        )
        .expect("failed to run trait methods query"),
    )
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "GenericStruct".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "GenericStruct".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "GenericStruct".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "GenericEnum".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "GenericEnum".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "GenericEnum".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "GenericUnion".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "GenericUnion".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "GenericUnion".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "GenericTrait".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "GenericTrait".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "GenericTrait".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "method".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'b".into(),
        },
        Output {
            name: "method".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "U".into(),
        },
        Output {
            name: "method".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "M".into(),
        },
        Output {
            name: "generic_fn".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "generic_fn".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "generic_fn".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "impl_trait".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'a".into(),
        },
        Output {
            name: "impl_trait".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "impl_trait".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
        Output {
            name: "impl_trait".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "impl GenericTrait<'a, T, N>".into(),
        },
        Output {
            name: "non_included_bound".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "explicit_where_bound".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "DefaultGenerics".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "DefaultGenerics".into(),
            generic_kind: "GenericConstParameter".into(),
            generic_name: "N".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn generic_type_parameters() {
    let path = "./localdata/test_data/generic_parameters/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let top_level_query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                name @output

                # TODO: HACK, remove this -- workaround for issue:
                # https://github.com/obi1kenobi/trustfall-rustdoc-adapter/issues/400
                #
                # This clause ensures this query doesn't return methods while #400 isn't resolved.
                name @filter(op: "!=", value: ["$method_name"])

                generic_parameter {
                    ... on GenericTypeParameter {
                        generic_name: name @output
                        synthetic @output
                        has_default @output

                        type_bound @fold {
                            bound: name @output
                        }
                    }
                }
            }
        }
    }
}
"#;
    let impl_owner_methods_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                impl {
                    method {
                        name @output

                        generic_parameter {
                            ... on GenericTypeParameter {
                                generic_name: name @output
                                synthetic @output
                                has_default @output

                                type_bound @fold {
                                    bound: name @output
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;
    let trait_methods_query = r#"
{
    Crate {
        item {
            ... on Trait {
                method {
                    name @output

                    generic_parameter {
                        ... on GenericTypeParameter {
                            generic_name: name @output
                            synthetic @output
                            has_default @output

                            type_bound @fold {
                                bound: name @output
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, i64> = BTreeMap::default();
    let mut top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();
    top_level_variables.insert("method_name", "method");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        generic_name: String,
        synthetic: bool,
        has_default: bool,
        bound: Vec<String>,
    }

    let mut results: Vec<_> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        top_level_query,
        top_level_variables.clone(),
    )
    .expect("failed to run top level query")
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            impl_owner_methods_query,
            variables.clone(),
        )
        .expect("failed to run impl owners query"),
    )
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            trait_methods_query,
            variables.clone(),
        )
        .expect("failed to run trait methods query"),
    )
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "GenericStruct".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "GenericEnum".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "GenericUnion".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "GenericTrait".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "method".into(),
            generic_name: "U".into(),
            synthetic: false,
            has_default: false,
            bound: ["Hash"].into_iter().map(ToString::to_string).collect(),
        },
        Output {
            name: "generic_fn".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Clone", "PartialOrd"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "impl GenericTrait<'a, T, N>".into(),
            synthetic: true,
            has_default: false,
            bound: ["GenericTrait"]
                .into_iter()
                .map(ToString::to_string)
                .collect(),
        },
        Output {
            name: "non_included_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: ["Unpin"].into_iter().map(ToString::to_string).collect(),
        },
        Output {
            name: "explicit_where_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: {
                // TODO: FIXME, the correct bound here should be ["Iterator"]
                //       but because we only partially inline built-in trait definitions,
                //       `Iterator` is not resolved and is invisible.
                //       When that's fixed, switch implementations to:
                //
                // ["Iterator"].into_iter().map(ToString::to_string).collect(),
                vec![]
            },
        },
        Output {
            name: "DefaultGenerics".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: true,
            bound: ["Copy"].into_iter().map(ToString::to_string).collect(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn generic_const_parameters() {
    let path = "./localdata/test_data/generic_parameters/rustdoc.json";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");

    let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
    let indexed_crate = IndexedCrate::new(&crate_);
    let adapter = Arc::new(RustdocAdapter::new(&indexed_crate, None));

    let top_level_query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                name @output

                # TODO: HACK, remove this -- workaround for issue:
                # https://github.com/obi1kenobi/trustfall-rustdoc-adapter/issues/400
                #
                # This clause ensures this query doesn't return methods while #400 isn't resolved.
                name @filter(op: "!=", value: ["$method_name"])

                generic_parameter {
                    ... on GenericConstParameter {
                        generic_name: name @output
                        has_default @output
                    }
                }
            }
        }
    }
}
"#;
    let impl_owner_methods_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                impl {
                    method {
                        name @output

                        generic_parameter {
                            ... on GenericConstParameter {
                                generic_name: name @output
                                has_default @output
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;
    let trait_methods_query = r#"
{
    Crate {
        item {
            ... on Trait {
                method {
                    name @output

                    generic_parameter {
                        ... on GenericConstParameter {
                            generic_name: name @output
                            has_default @output
                        }
                    }
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, i64> = BTreeMap::default();
    let mut top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();
    top_level_variables.insert("method_name", "method");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        generic_name: String,
        has_default: bool,
    }

    let mut results: Vec<_> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        top_level_query,
        top_level_variables.clone(),
    )
    .expect("failed to run top level query")
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            impl_owner_methods_query,
            variables.clone(),
        )
        .expect("failed to run impl owners query"),
    )
    .chain(
        trustfall::execute_query(
            &schema,
            adapter.clone(),
            trait_methods_query,
            variables.clone(),
        )
        .expect("failed to run trait methods query"),
    )
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "GenericStruct".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        Output {
            name: "GenericEnum".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        Output {
            name: "GenericUnion".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        Output {
            name: "GenericTrait".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        // TODO: The below items in principle should only be reachable via the trait's contents,
        //       not from top-level. This is unintentional behavior on the part of the adapter
        //       due to code unrelated to what we're testing here.
        //       When that change is applied, we'll need separate test queries
        //       for generic methods that navigate both via `ImplOwner` and via `Trait`.
        Output {
            name: "method".into(),
            generic_name: "M".into(),
            has_default: false,
        },
        // ^ end TODO region ^
        Output {
            name: "generic_fn".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "N".into(),
            has_default: false,
        },
        Output {
            name: "DefaultGenerics".into(),
            generic_name: "N".into(),
            has_default: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}
