// The Trustfall API requires the adapter to be passed in as an Arc.
// Our adapter is not Send/Sync (it doesn't need it),
// but there's currently nothing we can do about this lint.
#![allow(clippy::arc_with_non_send_sync)]

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use maplit::btreemap;
use trustfall::{FieldValue, Schema, TryIntoStruct};

use crate::RustdocAdapter;

#[allow(dead_code)]
mod type_level_invariants {
    use crate::{IndexedCrate, PackageIndex, RustdocAdapter};

    fn ensure_send_and_sync<T: Send + Sync>(_value: &T) {}

    fn ensure_indexed_crate_is_send_and_sync(value: &IndexedCrate<'_>) {
        ensure_send_and_sync(value);
    }

    fn ensure_crate_handler_is_send_and_sync(value: &PackageIndex<'_>) {
        ensure_send_and_sync(value);
    }

    fn ensure_adapter_is_send_and_sync(value: &RustdocAdapter<'_>) {
        ensure_send_and_sync(value);
    }
}

// This has to be a macro due to borrows. It can't be a function call
// unless we get a "super let" feature in Rust.
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
    get_test_data!(data, impl_for_ref);
    let adapter = RustdocAdapter::new(&data, None);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    trustfall::provider::check_adapter_invariants(&schema, &adapter)
}

/// Ensure that methods implemented on references (like `&Foo`) show up in queries.
#[test]
fn impl_for_ref() {
    get_test_data!(data, impl_for_ref);
    let adapter = RustdocAdapter::new(&data, None);

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
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
    get_test_data!(data, supertrait);
    let adapter = RustdocAdapter::new(&data, None);

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
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
    get_test_data!(data, sealed_traits);
    let adapter = RustdocAdapter::new(&data, None);

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
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
    get_test_data!(data, consts);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, traits_with_associated_types);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, statics);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, modules);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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

    // Ensure both the rows and the folded paths within each row come
    // in a consistent, deterministic order.
    results.sort_unstable();
    results.iter_mut().for_each(|row| row.paths.sort());

    similar_asserts::assert_eq!(
        vec![
            Output {
                module: "hello".into(),
                is_stripped: false,
                members: vec![Some("world".into()), Some("T2".into())],
                types: vec!["Module".into(), "Struct".into()],
                paths: vec![
                    vec!["modules".into(), "hello".into()],
                    vec!["modules".into(), "hi".into()],
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
    get_test_data!(data, associated_consts);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, function_abi);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, function_export_name);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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

    // Ensure that looking up functions by export name works correctly,
    // since this path is expected to hit our index instead of iterating over everything.
    let query = r#"
    {
        Crate {
            item {
                ... on Function {
                    name @output
                    export_name @filter(op: "=", value: ["$export_name"]) @output
                    visibility_limit @output
                }
            }
        }
    }
    "#;
    for row in results {
        let Some(export_name) = &row.export_name else {
            continue;
        };
        let variables: BTreeMap<&str, &str> = [("export_name", export_name.as_str())]
            .into_iter()
            .collect();

        let mut results: Vec<_> =
            trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
                .expect("failed to run query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.sort_unstable();

        similar_asserts::assert_eq!(vec![row], results);
    }
}

#[test]
fn importable_paths() {
    get_test_data!(data, importable_paths);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, importable_paths);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, importable_paths);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, importable_paths);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, unions);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
    get_test_data!(data, function_has_body);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    // This query should only return functions defined at top level,
    // not ones inside traits or `impl` blocks. Those are supposed to be of type `Method` instead.
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
            name: "extern_no_body".into(),
            has_body: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

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
    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct OutputWithOwner {
        owner: String,
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
        OutputWithOwner {
            owner: "Bar".into(),
            name: "trait_no_body".into(),
            has_body: false,
        },
        OutputWithOwner {
            owner: "Bar".into(),
            name: "trait_with_body".into(),
            has_body: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                owner: name @output

                inherent_impl {
                    method {
                        name @output
                        has_body @output
                    }
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

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![OutputWithOwner {
        owner: "Foo".into(),
        name: "inside_impl_block".into(),
        has_body: true,
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_discriminants() {
    get_test_data!(data, enum_discriminants);
    let adapter = RustdocAdapter::new(&data, None);

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
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
    get_test_data!(data, declarative_macros);
    let adapter = RustdocAdapter::new(&data, None);

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
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
    get_test_data!(data, proc_macros);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on ProcMacro {
                kind: __typename @output
                name @output
                public_api_eligible @output
                visibility_limit @output

                importable_path {
                    path @output
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
        kind: String,
        name: String,
        public_api_eligible: bool,
        visibility_limit: String,
        path: Vec<String>,
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
            kind: "FunctionLikeProcMacro".into(),
            name: "make_answer".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            path: vec!["proc_macros".into(), "make_answer".into()],
            public_api: true,
        },
        Output {
            kind: "AttributeProcMacro".into(),
            name: "return_as_is".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            path: vec!["proc_macros".into(), "return_as_is".into()],
            public_api: true,
        },
        Output {
            kind: "DeriveProcMacro".into(),
            name: "AnswerFn".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            path: vec!["proc_macros".into(), "AnswerFn".into()],
            public_api: true,
        },
        Output {
            kind: "DeriveProcMacro".into(),
            name: "HelperAttr".into(),
            public_api_eligible: true,
            visibility_limit: "public".into(),
            path: vec!["proc_macros".into(), "HelperAttr".into()],
            public_api: true,
        },
        Output {
            kind: "FunctionLikeProcMacro".into(),
            name: "hidden".into(),
            public_api_eligible: false,
            visibility_limit: "public".into(),
            path: vec!["proc_macros".into(), "hidden".into()],
            public_api: false,
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
    get_test_data!(data, generic_parameters);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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
            name: "combined_explicit_where_bound".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "complex_explicit_where_bound".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "combined_bounds".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "full_path_trait_bound".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "T".into(),
        },
        Output {
            name: "renamed_trait_bound".into(),
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
    get_test_data!(data, generic_parameters);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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

    let mut results: Vec<Output> = trustfall::execute_query(
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

    // Ensure that the results are in sorted order, and also that the aggregated bounds are sorted.
    results.sort_unstable();
    results.iter_mut().for_each(|row| row.bound.sort_unstable());

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
            bound: { ["Iterator"].into_iter().map(ToString::to_string).collect() },
        },
        Output {
            name: "combined_explicit_where_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: {
                ["Clone", "Iterator"]
                    .into_iter()
                    .map(ToString::to_string)
                    .collect()
            },
        },
        Output {
            name: "complex_explicit_where_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: { ["Iterator"].into_iter().map(ToString::to_string).collect() },
        },
        Output {
            name: "combined_bounds".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: {
                ["Clone", "Iterator"]
                    .into_iter()
                    .map(ToString::to_string)
                    .collect()
            },
        },
        Output {
            name: "full_path_trait_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: { ["Debug"].into_iter().map(ToString::to_string).collect() },
        },
        Output {
            name: "renamed_trait_bound".into(),
            generic_name: "T".into(),
            synthetic: false,
            has_default: false,
            bound: { ["Write"].into_iter().map(ToString::to_string).collect() },
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
    get_test_data!(data, generic_parameters);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

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

#[test]
fn implemented_trait_instantiated_name() {
    get_test_data!(data, rust_type_name);
    let adapter = RustdocAdapter::new(&data, None);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @filter(op: "=", value: ["$struct"])

                impl {
                    implemented_trait {
                        bare_name @output @filter(op: "one_of", value: ["$traits"])
                        instantiated_name @output
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, FieldValue> = BTreeMap::default();
    variables.insert("struct", "A".into());
    variables.insert(
        "traits",
        vec![
            "MyTrait",
            "MyTrait2",
            "Any",
            "Borrow",
            "BorrowMut",
            "From",
            "Into",
            "RefUnwindSafe",
            "Send",
            "Sync",
            "TryFrom",
            "TryInto",
            "Unpin",
            "UnwindSafe",
        ]
        .into(),
    );

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        bare_name: String,
        instantiated_name: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables.clone())
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
            bare_name: "Any".into(),
            instantiated_name: "Any".into(),
        },
        Output {
            bare_name: "Borrow".into(),
            instantiated_name: "Borrow<T>".into(),
        },
        Output {
            bare_name: "BorrowMut".into(),
            instantiated_name: "BorrowMut<T>".into(),
        },
        Output {
            bare_name: "From".into(),
            instantiated_name: "From<T>".into(),
        },
        Output {
            bare_name: "Into".into(),
            instantiated_name: "Into<U>".into(),
        },
        Output {
            bare_name: "MyTrait".into(),
            instantiated_name: "MyTrait".into(),
        },
        Output {
            bare_name: "MyTrait2".into(),
            instantiated_name: "MyTrait2<'a, N, i64>".into(),
        },
        Output {
            bare_name: "RefUnwindSafe".into(),
            instantiated_name: "RefUnwindSafe".into(),
        },
        Output {
            bare_name: "Send".into(),
            instantiated_name: "Send".into(),
        },
        Output {
            bare_name: "Sync".into(),
            instantiated_name: "Sync".into(),
        },
        Output {
            bare_name: "TryFrom".into(),
            instantiated_name: "TryFrom<U>".into(),
        },
        Output {
            bare_name: "TryInto".into(),
            instantiated_name: "TryInto<U>".into(),
        },
        Output {
            bare_name: "Unpin".into(),
            instantiated_name: "Unpin".into(),
        },
        Output {
            bare_name: "UnwindSafe".into(),
            instantiated_name: "UnwindSafe".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn parenthesized_type_bounds_on_type_and_impl() {
    get_test_data!(data, rust_type_name);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @filter(op: "=", value: ["$struct"]) @output

                generic_parameter {
                    ... on GenericTypeParameter {
                        generic: name @output
                        bound_: type_bound {
                            instantiated_name @output
                        }
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, &str> = BTreeMap::default();
    variables.insert("struct", "ParenthesizedGenericType");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        generic: String,
        bound_instantiated_name: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![Output {
        name: "ParenthesizedGenericType".into(),
        generic: "T".into(),
        bound_instantiated_name: "for<'a> Fn(&'a i64) -> &'a i64".into(),
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @filter(op: "=", value: ["$struct"]) @output

                generic_parameter {
                    ... on GenericTypeParameter {
                        generic: name @output
                    }
                }

                impl_: inherent_impl {
                    generic_parameter {
                        ... on GenericTypeParameter {
                            generic: name @output
                            bound_: type_bound {
                                instantiated_name @output
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, &str> = BTreeMap::default();
    variables.insert("struct", "ParenthesizedGenericImpl");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct LatterOutput {
        name: String,
        generic: String,
        impl_generic: String,
        impl_bound_instantiated_name: String,
    }

    let mut results: Vec<LatterOutput> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![LatterOutput {
        name: "ParenthesizedGenericImpl".into(),
        generic: "T".into(),
        impl_generic: "T".into(),
        impl_bound_instantiated_name: "for<'a> Fn(&'a i64) -> &'a i64".into(),
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn features() {
    get_test_data!(data, features);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        feature {
            name @output

            directly_enables @optional {
                enables: name @output
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
        enables: Option<String>,
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
            name: "default".into(),
            enables: Some("foo".into()),
        },
        Output {
            name: "default".into(),
            enables: Some("bar".into()),
        },
        Output {
            name: "foo".into(),
            enables: Some("baz".into()),
        },
        Output {
            name: "bar".into(),
            enables: None,
        },
        Output {
            name: "baz".into(),
            enables: None,
        },
        Output {
            name: "opt_in".into(),
            enables: None,
        },
        Output {
            name: "serde".into(),
            enables: None,
        },
        Output {
            name: "serde_json".into(),
            enables: None,
        },
        Output {
            name: "nightly".into(),
            enables: None,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    //
    // Check default features as well.
    //

    let query = r#"
{
    Crate {
        default_feature {
            name @output
        }
    }
}
"#;

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct DefaultsOutput {
        name: String,
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
        DefaultsOutput {
            name: "default".into(),
        },
        DefaultsOutput { name: "foo".into() },
        DefaultsOutput { name: "bar".into() },
        DefaultsOutput { name: "baz".into() },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn type_generic_bounds() {
    get_test_data!(data, type_generic_bounds);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                kind: __typename @output
                name @output

                generic_parameter {
                    ... on GenericTypeParameter {
                        generic: name @output

                        type_bound {
                            bound: instantiated_name @output
                        }
                    }
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
        kind: String,
        name: String,
        generic: String,
        bound: String,
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
            kind: "Struct".into(),
            name: "ExampleStruct".into(),
            generic: "T".into(),
            bound: "Ord".into(),
        },
        Output {
            kind: "Enum".into(),
            name: "ExampleEnum".into(),
            generic: "T".into(),
            bound: "PartialEq".into(),
        },
        Output {
            kind: "Enum".into(),
            name: "ExampleEnum".into(),
            generic: "T".into(),
            bound: "Sync".into(),
        },
        Output {
            kind: "Union".into(),
            name: "ExampleUnion".into(),
            generic: "T".into(),
            bound: "std::fmt::Debug".into(),
        },
        Output {
            kind: "Union".into(),
            name: "ExampleUnion".into(),
            generic: "T".into(),
            bound: "Copy".into(),
        },
        Output {
            kind: "Struct".into(),
            name: "IteratorWrapper".into(),
            generic: "T".into(),
            bound: "Sync".into(),
        },
        Output {
            kind: "Struct".into(),
            name: "IteratorWrapper".into(),
            generic: "T".into(),
            bound: "Iterator<Item = i64>".into(),
        },
        Output {
            kind: "Struct".into(),
            name: "LifetimedIterator".into(),
            generic: "T".into(),
            // `T: Iterator<Item = &'a str> + 'a` is equivalent to:
            // ```
            // where
            //   T: Iterator<Item = &'a str>,
            //   T: 'a
            // ```
            // and only the `Iterator` portion is a *type* bound.
            bound: "Iterator<Item = &'a str>".into(),
        },
        Output {
            kind: "Struct".into(),
            name: "SeparateIteratorBounds".into(),
            generic: "T".into(),
            // confirming the equivalence of the previous case
            bound: "Iterator<Item = &'a str>".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_signatures() {
    get_test_data!(data, raw_type_json);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output @filter(op: "=", value: ["$func"])
                signature @output
            }
        }
    }
}
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::from_iter([("func", "awesome_function")]);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "awesome_function".into(),
            signature: "fn awesome_function<'a, const N: usize>(a: &'a Constant<N>, b: &impl Clone) -> impl Send".into()
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn method_signature() {
    get_test_data!(data, raw_type_json);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])
                method {
                    name @output
                    signature @output
                }
            }
        }
    }
}
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::from_iter([("trait", "MyTrait")]);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "associated_types".into(),
            signature:
                "fn associated_types<T, U>(a: Self::Assoc<T>, b: <Self as MyTrait>::Assoc<U>) \
                    where Self::Assoc<()>: Send + 'static"
                    .into(),
        },
        Output {
            name: "method".into(),
            signature: "fn method<'a, T, U: GAT<(T, ())>>() where Self: Sized,\n\
                for<'b> <U as GAT<(T, ())>>::Type<'b, ()>: 'static"
                .into(),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn trait_method_nested_generics() {
    get_test_data!(data, raw_type_json);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])
                method {
                    name @output
                    signature @output
                }
            }
        }
    }
}
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::from_iter([("trait", "GenericTrait")]);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![Output {
        name: "nested_generics".into(),
        signature: "fn nested_generics<U>(t: T, u: U)".into(),
    }];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn extern_fn() {
    get_test_data!(data, extern_fn);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output
                is_unsafe: unsafe @output
                has_body @output

                importable_path @optional {
                    public_api @output
                    path @output
                }
            }
        }
    }
}
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::new();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        is_unsafe: bool,
        has_body: bool,
        public_api: Option<bool>,
        path: Option<Vec<String>>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "legacy_extern_fn".into(),
            is_unsafe: true,
            has_body: false,
            public_api: Some(true),
            path: Some(vec!["extern_fn".into(), "legacy_extern_fn".into()]),
        },
        Output {
            name: "implicit_unsafe_extern_fn".into(),
            is_unsafe: true,
            has_body: false,
            public_api: Some(true),
            path: Some(vec!["extern_fn".into(), "implicit_unsafe_extern_fn".into()]),
        },
        Output {
            name: "explicit_unsafe_extern_fn".into(),
            is_unsafe: true,
            has_body: false,
            public_api: Some(true),
            path: Some(vec!["extern_fn".into(), "explicit_unsafe_extern_fn".into()]),
        },
        Output {
            name: "safe_extern_fn".into(),
            is_unsafe: false,
            has_body: false,
            public_api: Some(true),
            path: Some(vec!["extern_fn".into(), "safe_extern_fn".into()]),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn item_lookup_by_path_optimization() {
    // Any test crate with non-public top-level items would work for this test.
    get_test_data!(data, associated_consts);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                # Since this edge is optional, this query matches:
                # - functions with an importable path matching the filter, and
                # - functions that *do not have* importable paths at all.
                #
                # Failure to return both of these means we have a bug in
                # the "item lookup by importable path" optimization code path.
                importable_path @optional {
                    public_api @output
                    path @output @filter(op: "=", value: ["$path"])
                }
            }
        }
    }
}
    "#;

    let variables = btreemap! {
        "path" => vec!["associated_consts", "will_not_match", "anything"],
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        public_api: Option<bool>,
        path: Option<Vec<String>>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![Output {
        name: "min_batch_size".into(),
        public_api: None,
        path: None,
    }];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn impl_lookup_by_method_name_optimization() {
    // Any test crate that has `impl` blocks without methods would work for this test.
    get_test_data!(data, associated_consts);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                name @output

                # Since this edge is optional, this query matches:
                # - types with inherent impls containing the named method
                # - types that *do not have* any methods in their inherent impls.
                #
                # Failure to account for either of these cases means we have a bug in
                # the "item lookup by importable path" optimization code path.
                inherent_impl {
                    method @optional {
                        method: name @output @filter(op: "=", value: ["$name"])
                    }
                }
            }
        }
    }
}
    "#;

    let variables = btreemap! {
        "name" => "non_existent",
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        method: Option<String>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![Output {
        name: "Counter".into(),
        method: None,
    }];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn generic_param_positions() {
    get_test_data!(data, generic_param_positions);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                item: name @output
                item_kind: __typename @output

                generic_parameter {
                    name @output
                    kind: __typename @output
                    position @output
                }
            }
        }
    }
}
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::new();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        item_kind: String,
        item: String,
        name: String,
        kind: String,
        position: Option<i64>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "'b".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "T".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "U".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "N".into(),
            kind: "GenericConstParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "M".into(),
            kind: "GenericConstParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            name: "T".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            name: "N".into(),
            kind: "GenericConstParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "impl_trait".into(),
            name: "T".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "impl_trait".into(),
            name: "U".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Function".into(),
            item: "impl_trait".into(),
            name: "impl Into<U>".into(),
            kind: "GenericTypeParameter".into(),
            position: None,
        },
        Output {
            item_kind: "Struct".into(),
            item: "Example".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Struct".into(),
            item: "Example".into(),
            name: "'b".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Struct".into(),
            item: "SingleLifetimeElided".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                item: name @output
                item_kind: __typename @output

                inherent_impl {
                    generic_parameter {
                        name @output
                        kind: __typename @output
                        position @output
                    }
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

    let mut expected_results = vec![
        Output {
            item_kind: "Struct".into(),
            item: "Example".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Struct".into(),
            item: "Example".into(),
            name: "'b".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(2),
        },
        // `impl SingleLifetimeElided<'_>` doesn't have a generic parameter,
        // since neither implicit nor elided parameters get an entry.
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                item: name @output
                item_kind: __typename @output

                inherent_impl {
                    method {
                        method: name @output

                        generic_parameter {
                            name @output
                            kind: __typename @output
                            position @output
                        }
                    }
                }
            }
        }
    }
}
    "#;

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output2 {
        item_kind: String,
        item: String,
        method: String,
        name: String,
        kind: String,
        position: Option<i64>,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output2 {
            item_kind: "Struct".into(),
            item: "Example".into(),
            method: "elided_lifetimes".into(),
            name: "'c".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output2 {
            item_kind: "Struct".into(),
            item: "SingleLifetimeElided".into(),
            method: "explicit_self_lifetime".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output2 {
            item_kind: "Struct".into(),
            item: "SingleLifetimeElided".into(),
            method: "explicit_self_lifetime".into(),
            name: "impl Into<&'a i64>".into(),
            kind: "GenericTypeParameter".into(),
            position: None,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                item: name @output
                item_kind: __typename @output

                method {
                    method: name @output

                    generic_parameter {
                        name @output
                        kind: __typename @output
                        position @output
                    }
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

    let mut expected_results = vec![
        Output2 {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            method: "method".into(),
            name: "'b".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output2 {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            method: "method".into(),
            name: "U".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(1),
        },
        Output2 {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            method: "method".into(),
            name: "V".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(2),
        },
        Output2 {
            item_kind: "Trait".into(),
            item: "Trait".into(),
            method: "method".into(),
            name: "M".into(),
            kind: "GenericConstParameter".into(),
            position: Some(1),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}
