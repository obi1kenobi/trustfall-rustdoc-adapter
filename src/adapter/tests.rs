// The Trustfall API requires the adapter to be passed in as an Arc.
// Our adapter is not Send/Sync (it doesn't need it),
// but there's currently nothing we can do about this lint.
#![allow(clippy::arc_with_non_send_sync)]

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::Context;
use maplit::btreemap;
use trustfall::{FieldValue, Schema, TryIntoStruct};

use crate::{PackageIndex, RustdocAdapter};

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

// This mirrors `get_test_data!` for fixtures that intentionally need the
// Rust standard-library indexing policy instead of ordinary crate indexing.
macro_rules! get_rust_std_test_data {
    ($data:ident, $case:ident) => {
        let crate_ = crate::test_util::load_pregenerated_rustdoc(stringify!($case));
        let $data = crate::PackageIndex::from_rust_std_component_crate(&crate_);
    };
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
fn rustdoc_finds_where_self_supertraits() {
    get_test_data!(data, supertrait_where_self);
    let adapter = RustdocAdapter::new(&data, None);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @output @filter(op: "one_of", value: ["$traits"])

                supertrait {
                    supertrait: name @output
                    instantiated_name @output
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, FieldValue> = BTreeMap::default();
    variables.insert(
        "traits",
        vec![
            "HeaderSupertrait",
            "MixedSuperAndNonSuper",
            "NonSelfWhere",
            "RefSelfWithTraitLifetime",
            "RefSelfWhere",
            "TwoColonTwoWhere",
            "WhereSelfGeneric",
            "WhereSelfSupertrait",
        ]
        .into(),
    );

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        supertrait: String,
        instantiated_name: String,
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
                name: "HeaderSupertrait".into(),
                supertrait: "Base".into(),
                instantiated_name: "Base<Assoc = u8>".into(),
            },
            Output {
                name: "MixedSuperAndNonSuper".into(),
                supertrait: "Base".into(),
                instantiated_name: "Base<Assoc = u8>".into(),
            },
            Output {
                name: "MixedSuperAndNonSuper".into(),
                supertrait: "GenericBase".into(),
                instantiated_name: "GenericBase<T>".into(),
            },
            Output {
                name: "TwoColonTwoWhere".into(),
                supertrait: "Base".into(),
                instantiated_name: "Base<Assoc = u8>".into(),
            },
            Output {
                name: "TwoColonTwoWhere".into(),
                supertrait: "GenericBase".into(),
                instantiated_name: "GenericBase<T>".into(),
            },
            Output {
                name: "TwoColonTwoWhere".into(),
                supertrait: "GenericMarker".into(),
                instantiated_name: "GenericMarker<T>".into(),
            },
            Output {
                name: "TwoColonTwoWhere".into(),
                supertrait: "LocalMarker".into(),
                instantiated_name: "LocalMarker".into(),
            },
            Output {
                name: "WhereSelfGeneric".into(),
                supertrait: "GenericBase".into(),
                instantiated_name: "GenericBase<T>".into(),
            },
            Output {
                name: "WhereSelfSupertrait".into(),
                supertrait: "Base".into(),
                instantiated_name: "Base<Assoc = u8>".into(),
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
                public_api_sealed @output

                importable_path @fold {
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
        path: Vec<Vec<String>>,
        sealed: bool,
        public_api_sealed: bool,
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
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "InternalMarker".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "DirectlyTraitSealed".into(),
            path: vec![vec!["sealed_traits".into(), "DirectlyTraitSealed".into()]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "TransitivelyTraitSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "TransitivelyTraitSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "SealedTraitWithStdSupertrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "SealedTraitWithStdSupertrait".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "SealedWithWhereSelfBound".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "SealedWithWhereSelfBound".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "PrivateSealed".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "SealedWithPrivateSupertrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "SealedWithPrivateSupertrait".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "Unsealed".into(),
            path: vec![vec!["sealed_traits".into(), "Unsealed".into()]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "MethodSealed".into(),
            path: vec![vec!["sealed_traits".into(), "MethodSealed".into()]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "MethodReturnSealed".into(),
            path: vec![vec!["sealed_traits".into(), "MethodReturnSealed".into()]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "TransitivelyMethodSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "TransitivelyMethodSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "NotMethodSealedBecauseOfDefaultImpl".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "NotMethodSealedBecauseOfDefaultImpl".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "ConstItemPubInPrivTypeSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "ConstItemPubInPrivTypeSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "NotSealedDueToConstDefaultValue".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "NotSealedDueToConstDefaultValue".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "NotTransitivelySealed".into(),
            path: vec![vec!["sealed_traits".into(), "NotTransitivelySealed".into()]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "TraitUnsealedButMethodGenericSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "TraitUnsealedButMethodGenericSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "NotGenericSealedBecauseOfDefaultImpl".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "NotGenericSealedBecauseOfDefaultImpl".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "IteratorExt".into(),
            path: vec![vec!["sealed_traits".into(), "IteratorExt".into()]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "Iterator".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "shadow_builtins".into(),
                "Iterator".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "ShadowedSubIterator".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "shadow_builtins".into(),
                "ShadowedSubIterator".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "Super".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "generic_seal".into(),
                "Super".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "Marker".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "NotGenericSealedBecauseOfPubSupertrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "generic_seal".into(),
                "NotGenericSealedBecauseOfPubSupertrait".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "FullBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "PrivateBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "RefBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "ExternalSupertraitsBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketWithWhereClause".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "IteratorBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverLocalUnsealedTrait".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSealedTrait".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSealedAndUnsealedTrait".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "TransitiveBlanket".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArc".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverTuple".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSlice".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArray".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverPointer".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketUnsealed".into(),
            path: vec![vec!["sealed_traits".into(), "BlanketUnsealed".into()]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "RefBlanketUnsealed".into(),
            path: vec![vec!["sealed_traits".into(), "RefBlanketUnsealed".into()]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "ExternalSupertraitsBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "ExternalSupertraitsBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketWithWhereClauseUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketWithWhereClauseUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "IteratorBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "IteratorBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverLocalUnsealedTraitUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverLocalUnsealedTraitUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverSealedTraitSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverSealedTraitSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketSealedOverMultiple".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketSealedOverMultiple".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "TransitiveBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "TransitiveBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverArcSealed".into(),
            path: vec![vec!["sealed_traits".into(), "BlanketOverArcSealed".into()]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverTupleSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverTupleSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSliceSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverSliceSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArraySealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverArraySealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverPointerSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "BlanketOverPointerSealed".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "RecursiveSealed".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "SealedPlusRecursiveBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "cyclic_bounds".into(),
                "SealedPlusRecursiveBlanket".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "NonRefRecursiveSealed".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "NonRefSealedPlusRecursiveBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "cyclic_bounds2".into(),
                "NonRefSealedPlusRecursiveBlanket".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "DirectCycleSuper".into(),
            path: vec![],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "DirectCycleSub".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "direct_cycle".into(),
                "DirectCycleSub".into(),
            ]],
            sealed: true,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "hidden_module".into(),
                "HiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "Unsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "Unsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "DirectlyHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "DirectlyHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenSealedInherited".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenSealedInherited".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "TransitivelyHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "TransitivelyHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenSealedWithWhereSelfBound".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenSealedWithWhereSelfBound".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "MethodHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "MethodHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "MethodReturnHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "MethodReturnHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenMethodHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenMethodHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "TransitivelyMethodHiddenSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "TransitivelyMethodHiddenSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "NotMethodHiddenSealedBecauseOfDefaultImpl".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "NotMethodHiddenSealedBecauseOfDefaultImpl".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "HiddenSealedAssocType".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenSealedAssocType".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenSealedAssocConst".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenSealedAssocConst".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "HiddenSealedAssocConstType".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "HiddenSealedAssocConstType".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "UnsealedDefaultAssocConst".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "UnsealedDefaultAssocConst".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "MethodWithHiddenBound".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "MethodWithHiddenBound".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "FullBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "FullBlanket".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "RefBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "RefBlanket".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "ExternalSupertraitsBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "ExternalSupertraitsBlanket".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketWithWhereClause".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketWithWhereClause".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "IteratorBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "IteratorBlanket".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverLocalUnsealedTrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverLocalUnsealedTrait".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSealedTrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverSealedTrait".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSealedAndUnsealedTrait".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverSealedAndUnsealedTrait".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "TransitiveBlanket".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "TransitiveBlanket".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArc".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverArc".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverTuple".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverTuple".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSlice".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverSlice".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArray".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverArray".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverPointer".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "blanket_impls".into(),
                "BlanketOverPointer".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "RefBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "RefBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "ExternalSupertraitsBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "ExternalSupertraitsBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketWithWhereClauseUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketWithWhereClauseUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "IteratorBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "IteratorBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverLocalUnsealedTraitUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverLocalUnsealedTraitUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverSealedTraitSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverSealedTraitSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketSealedOverMultiple".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketSealedOverMultiple".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "TransitiveBlanketUnsealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "TransitiveBlanketUnsealed".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "BlanketOverArcSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverArcSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverTupleSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverTupleSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverSliceSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverSliceSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverArraySealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverArraySealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "BlanketOverPointerSealed".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "BlanketOverPointerSealed".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "DeprecatedHidden".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "DeprecatedHidden".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "UnsealedDueToDeprecatedSuper".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "UnsealedDueToDeprecatedSuper".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "DeprecatedAssocType".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "DeprecatedAssocType".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "DeprecatedAssocConst".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "DeprecatedAssocConst".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "DeprecatedMethod".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "DeprecatedMethod".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "MethodDeprecatedArgType".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "MethodDeprecatedArgType".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "DirectCycleSuper".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "direct_cycle".into(),
                "hidden".into(),
                "DirectCycleSuper".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "DirectCycleSub".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "doc_hidden".into(),
                "direct_cycle".into(),
                "DirectCycleSub".into(),
            ]],
            sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "Base".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "regression_csc_1200".into(),
                "Base".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "Left".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "regression_csc_1200".into(),
                "Left".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "Right".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "regression_csc_1200".into(),
                "Right".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "Top".into(),
            path: vec![vec![
                "sealed_traits".into(),
                "regression_csc_1200".into(),
                "Top".into(),
            ]],
            sealed: false,
            public_api_sealed: false,
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
                is_unsafe: unsafe @output

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
        is_unsafe: bool,
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
                is_unsafe: false,
            },
            Output {
                name: "MUT".into(),
                path: vec!["statics".into(), "MUT".into()],
                mutable: true,
                is_unsafe: false,
            },
            Output {
                name: "SAFE".into(),
                path: vec!["statics".into(), "SAFE".into()],
                mutable: false,
                is_unsafe: false,
            },
            Output {
                name: "SECOND".into(),
                path: vec!["statics".into(), "inner".into(), "SECOND".into()],
                mutable: false,
                is_unsafe: false,
            },
            Output {
                name: "UNSAFE".into(),
                path: vec!["statics".into(), "UNSAFE".into()],
                mutable: false,
                is_unsafe: true,
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
fn static_export_name() {
    get_test_data!(data2021, static_export_name_2021);
    get_test_data!(data, static_export_name);

    let adapter2021 = RustdocAdapter::new(&data2021, None);
    let adapter2021 = Arc::new(&adapter2021);

    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Static {
                name @output
                export_name @output
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
    }

    let expected_results = vec![
        Output {
            name: "VAR1".into(),
            export_name: Some("VAR1".into()),
        },
        Output {
            name: "VAR2".into(),
            export_name: Some("EXTERNALLY_VISIBLE".into()),
        },
        Output {
            name: "VAR3".into(),
            export_name: Some("EXTERNALLY_VISIBLE_3".into()),
        },
        Output {
            name: "VAR4".into(),
            export_name: Some("EXTERNALLY_VISIBLE_4".into()),
        },
    ];

    let mut results2021: Vec<Output> =
        trustfall::execute_query(&schema, adapter2021.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results2021.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results2021,);

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
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
    get_test_data!(data2021, function_export_name_2021);
    get_test_data!(data, function_export_name);

    let adapter2021 = RustdocAdapter::new(&data2021, None);
    let adapter2021 = Arc::new(&adapter2021);

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

    let mut expected_results = vec![
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
            name: "export_name_not_mangled".into(),
            export_name: Some("renamed_3".into()),
            visibility_limit: "crate".into(),
        },
        Output {
            name: "export_name_not_mangled_reversed".into(),
            export_name: Some("renamed_4".into()),
            visibility_limit: "crate".into(),
        },
        Output {
            name: "private_not_mangled".into(),
            export_name: Some("private_not_mangled".into()),
            visibility_limit: "crate".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results2021: Vec<_> =
        trustfall::execute_query(&schema, adapter2021.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results2021.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results2021,);

    // Ensure that looking up functions by export name works correctly,
    // since this path is expected to hit our index instead of iterating over everything.
    let inner_query = r#"
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
    for row in results2021 {
        let Some(export_name) = &row.export_name else {
            continue;
        };
        let inner_variables: BTreeMap<&str, &str> = [("export_name", export_name.as_str())]
            .into_iter()
            .collect();

        let mut results2021: Vec<_> = trustfall::execute_query(
            &schema,
            adapter2021.clone(),
            inner_query,
            inner_variables.clone(),
        )
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
        results2021.sort_unstable();

        similar_asserts::assert_eq!(vec![row], results2021);
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    // Ensure that looking up functions by export name works correctly,
    // since this path is expected to hit our index instead of iterating over everything.
    for row in results {
        let Some(export_name) = &row.export_name else {
            continue;
        };
        let inner_variables: BTreeMap<&str, &str> = [("export_name", export_name.as_str())]
            .into_iter()
            .collect();

        let mut results: Vec<_> = trustfall::execute_query(
            &schema,
            adapter.clone(),
            inner_query,
            inner_variables.clone(),
        )
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
            name: "HiddenGlobOnly".into(),
            path: vec!["importable_paths".into(), "HiddenGlobOnly".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "BothHiddenAndVisible".into(),
            path: vec!["importable_paths".into(), "BothHiddenAndVisible".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "BothHiddenAndVisible".into(),
            path: vec![
                "importable_paths".into(),
                "VisibleBothHiddenAndVisible".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "BothHiddenAndVisibleSameName".into(),
            path: vec![
                "importable_paths".into(),
                "BothHiddenAndVisibleSameName".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "HiddenSubmodulePathCanSortFirst".into(),
            path: vec![
                "importable_paths".into(),
                "hidden_glob_path_order_module".into(),
                "HiddenSubmodulePathCanSortFirst".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "HiddenSubmodulePathCanSortFirst".into(),
            path: vec![
                "importable_paths".into(),
                "VisibleHiddenSubmodulePathCanSortFirst".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "DuplicateGlobHiddenAndVisible".into(),
            path: vec![
                "importable_paths".into(),
                "DuplicateGlobHiddenAndVisible".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "DuplicateGlobHiddenDeprecatedAndVisible".into(),
            path: vec![
                "importable_paths".into(),
                "DuplicateGlobHiddenDeprecatedAndVisible".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "DuplicateGlobDeprecatedAndVisible".into(),
            path: vec![
                "importable_paths".into(),
                "DuplicateGlobDeprecatedAndVisible".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "DeprecatedGlobOnly".into(),
            path: vec!["importable_paths".into(), "DeprecatedGlobOnly".into()],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "HiddenDeprecatedGlobOnly".into(),
            path: vec!["importable_paths".into(), "HiddenDeprecatedGlobOnly".into()],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "NestedHiddenGlobOnly".into(),
            path: vec!["importable_paths".into(), "NestedHiddenGlobOnly".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "NestedHiddenDeprecatedGlobOnly".into(),
            path: vec![
                "importable_paths".into(),
                "NestedHiddenDeprecatedGlobOnly".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        Output {
            name: "PlainGlobOnly".into(),
            path: vec!["importable_paths".into(), "PlainGlobOnly".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "HiddenPerItemThenRootPerItem".into(),
            path: vec![
                "importable_paths".into(),
                "HiddenPerItemThenRootPerItem".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "HiddenPerItemThenRootGlob".into(),
            path: vec![
                "importable_paths".into(),
                "HiddenPerItemThenRootGlob".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "HiddenGlobThenRootPerItem".into(),
            path: vec![
                "importable_paths".into(),
                "HiddenGlobThenRootPerItem".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "HiddenGlobThenRootGlob".into(),
            path: vec!["importable_paths".into(), "HiddenGlobThenRootGlob".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
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

/// Ensure that if the same path is available via both a `#[doc(hidden)]` glob re-export
/// and also via some other manner that is public API, the net result is a valid public API path.
#[test]
fn hidden_glob_reexports_affect_synthesized_public_api_paths() {
    get_test_data!(data, doc_hidden_glob_reexports);
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
    let variables: BTreeMap<&str, &str> = Default::default();

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

    let mut results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "HiddenGlobOnly".into(),
            path: vec!["doc_hidden_glob_reexports".into(), "HiddenGlobOnly".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        Output {
            name: "HiddenAndVisibleGlob".into(),
            path: vec![
                "doc_hidden_glob_reexports".into(),
                "HiddenAndVisibleGlob".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        Output {
            name: "HiddenGlobAndDirectUse".into(),
            path: vec![
                "doc_hidden_glob_reexports".into(),
                "HiddenGlobAndDirectUse".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn normalized_paths_prefer_public_api_importable_path() {
    get_test_data!(data, importable_paths);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @filter(op: "=", value: ["$name"])

                return_value {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables = BTreeMap::from([("name", "hidden_glob_path_order_return")]);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        signature: String,
    }

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter.clone(), query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    similar_asserts::assert_eq!(
        vec![Output {
            signature: "::importable_paths::VisibleHiddenSubmodulePathCanSortFirst".into(),
        }],
        results,
    );
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
struct VariantImportablePath {
    name: String,
    path: Vec<String>,
    doc_hidden: bool,
    deprecated: bool,
    public_api: bool,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
struct ImportableItemPath {
    name: String,
    kind: String,
    path: Vec<String>,
}

fn collect_variant_importable_paths(data: &PackageIndex<'_>) -> Vec<VariantImportablePath> {
    let adapter = RustdocAdapter::new(data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Variant {
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

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    results
}

fn collect_module_importable_item_paths(
    data: &PackageIndex<'_>,
    module_name: &str,
) -> Vec<ImportableItemPath> {
    let adapter = RustdocAdapter::new(data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Module {
                name @filter(op: "=", value: ["$module"])
                item {
                    ... on Importable {
                        kind: __typename @output @filter(op: "one_of", value: ["$kinds"])
                        name @output
                        importable_path {
                            path @output
                        }
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, FieldValue> = BTreeMap::default();
    variables.insert("module", module_name.into());
    variables.insert("kinds", vec!["Struct", "Enum"].into());

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    results
}

/// Verifies enum variant importable paths across direct, renamed, doc-hidden, deprecated,
/// namespace, and glob shadowing cases in `enum_variant_imports`.
#[test]
fn enum_variant_importable_paths() {
    get_test_data!(data, enum_variant_imports);
    let results = collect_variant_importable_paths(&data);

    let mut expected_results = vec![
        VariantImportablePath {
            name: "Plain".into(),
            path: vec!["enum_variant_imports".into(), "Base".into(), "Plain".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Plain".into(),
            path: vec!["enum_variant_imports".into(), "Plain".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Plain".into(),
            path: vec!["enum_variant_imports".into(), "RenamedPlain".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Plain".into(),
            path: vec!["enum_variant_imports".into(), "HiddenPlain".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        VariantImportablePath {
            name: "Plain".into(),
            path: vec!["enum_variant_imports".into(), "DeprecatedPlain".into()],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        VariantImportablePath {
            name: "Deprecated".into(),
            path: vec![
                "enum_variant_imports".into(),
                "Base".into(),
                "Deprecated".into(),
            ],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        VariantImportablePath {
            name: "Deprecated".into(),
            path: vec!["enum_variant_imports".into(), "Deprecated".into()],
            doc_hidden: false,
            deprecated: true,
            public_api: true,
        },
        VariantImportablePath {
            name: "Hidden".into(),
            path: vec![
                "enum_variant_imports".into(),
                "Base".into(),
                "Hidden".into(),
            ],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        VariantImportablePath {
            name: "Hidden".into(),
            path: vec!["enum_variant_imports".into(), "Hidden".into()],
            doc_hidden: true,
            deprecated: false,
            public_api: false,
        },
        VariantImportablePath {
            name: "DeprecatedHidden".into(),
            path: vec![
                "enum_variant_imports".into(),
                "Base".into(),
                "DeprecatedHidden".into(),
            ],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        VariantImportablePath {
            name: "DeprecatedHidden".into(),
            path: vec!["enum_variant_imports".into(), "DeprecatedHidden".into()],
            doc_hidden: true,
            deprecated: true,
            public_api: true,
        },
        VariantImportablePath {
            name: "Red".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Colors".into(),
                "Red".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Red".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Red".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Green".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Colors".into(),
                "Green".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Blue".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Colors".into(),
                "Blue".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Clash".into(),
            path: vec![
                "enum_variant_imports".into(),
                "value_shadow".into(),
                "Shadowed".into(),
                "Clash".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Red".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Primary".into(),
                "Red".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Red".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Red".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Green".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Primary".into(),
                "Green".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Green".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Secondary".into(),
                "Green".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Blue".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Primary".into(),
                "Blue".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Blue".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Secondary".into(),
                "Blue".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Cyan".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Secondary".into(),
                "Cyan".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Cyan".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Cyan".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Same".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "Source".into(),
                "Same".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Same".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "Same".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "SameTuple".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "Source".into(),
                "SameTuple".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "SameTuple".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "SameTuple".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "SameStruct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "Source".into(),
                "SameStruct".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "SameStruct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_same_item".into(),
                "SameStruct".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Clash".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Left".into(),
                "Clash".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Tuple".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Left".into(),
                "Tuple".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Struct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Left".into(),
                "Struct".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Clash".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Right".into(),
                "Clash".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Tuple".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Right".into(),
                "Tuple".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Struct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Right".into(),
                "Struct".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "LeftOnly".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Left".into(),
                "LeftOnly".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "LeftOnly".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "LeftOnly".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "RightOnly".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "Right".into(),
                "RightOnly".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "RightOnly".into(),
            path: vec![
                "enum_variant_imports".into(),
                "glob_conflict".into(),
                "RightOnly".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Verifies importable paths for enum/struct items that drive variant shadowing and
/// glob-vs-glob ambiguity. We filter to the two modules that define those items,
/// since variant importable paths are validated separately and the full crate list
/// would be noisy.
#[test]
fn enum_variant_imports_module_item_paths() {
    get_test_data!(data, enum_variant_imports);

    let mut namespace_results = collect_module_importable_item_paths(&data, "namespace");
    let mut expected_namespace_results = vec![
        ImportableItemPath {
            name: "Blue".into(),
            kind: "Struct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Blue".into(),
            ],
        },
        ImportableItemPath {
            name: "Colors".into(),
            kind: "Enum".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Colors".into(),
            ],
        },
        ImportableItemPath {
            name: "Green".into(),
            kind: "Struct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Green".into(),
            ],
        },
        ImportableItemPath {
            name: "Red".into(),
            kind: "Struct".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace".into(),
                "Red".into(),
            ],
        },
    ];
    namespace_results.sort_unstable();
    expected_namespace_results.sort_unstable();
    similar_asserts::assert_eq!(expected_namespace_results, namespace_results);

    let mut conflict_results =
        collect_module_importable_item_paths(&data, "namespace_glob_conflict");
    let mut expected_conflict_results = vec![
        ImportableItemPath {
            name: "Primary".into(),
            kind: "Enum".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Primary".into(),
            ],
        },
        ImportableItemPath {
            name: "Secondary".into(),
            kind: "Enum".into(),
            path: vec![
                "enum_variant_imports".into(),
                "namespace_glob_conflict".into(),
                "Secondary".into(),
            ],
        },
    ];
    conflict_results.sort_unstable();
    expected_conflict_results.sort_unstable();
    similar_asserts::assert_eq!(expected_conflict_results, conflict_results);
}

/// Ensures glob reexports of enum variants surface as importable paths.
#[test]
fn enum_variant_glob_reexport_enum_variants() {
    get_test_data!(data, glob_reexport_enum_variants);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![
        VariantImportablePath {
            name: "First".into(),
            path: vec!["glob_reexport_enum_variants".into(), "First".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Second".into(),
            path: vec!["glob_reexport_enum_variants".into(), "Second".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Confirms glob-of-glob reexports preserve enum variant importability.
#[test]
fn enum_variant_glob_of_glob_reexport() {
    get_test_data!(data, glob_of_glob_reexport);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![VariantImportablePath {
        name: "First".into(),
        path: vec!["glob_of_glob_reexport".into(), "Baz".into(), "First".into()],
        doc_hidden: false,
        deprecated: false,
        public_api: true,
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Confirms glob-of-renamed reexports preserve enum variant importability.
#[test]
fn enum_variant_glob_of_renamed_reexport() {
    get_test_data!(data, glob_of_renamed_reexport);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![VariantImportablePath {
        name: "First".into(),
        path: vec!["glob_of_renamed_reexport".into(), "RenamedFirst".into()],
        doc_hidden: false,
        deprecated: false,
        public_api: true,
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Verifies that glob-reexporting both an enum and its variants yields both importable paths.
#[test]
fn enum_variant_glob_reexport_enum_and_contents() {
    get_test_data!(data, glob_reexport);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![
        VariantImportablePath {
            name: "First".into(),
            path: vec!["glob_reexport".into(), "First".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "First".into(),
            path: vec!["glob_reexport".into(), "Baz".into(), "First".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Second".into(),
            path: vec!["glob_reexport".into(), "Second".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Second".into(),
            path: vec!["glob_reexport".into(), "Baz".into(), "Second".into()],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Ensures enum variant glob reexports do not shadow same-named functions in other namespaces.
#[test]
fn enum_variant_glob_of_enum_does_not_shadow_local_fn() {
    get_test_data!(data, glob_of_enum_does_not_shadow_local_fn);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![VariantImportablePath {
        name: "First".into(),
        path: vec![
            "glob_of_enum_does_not_shadow_local_fn".into(),
            "Foo".into(),
            "First".into(),
        ],
        doc_hidden: false,
        deprecated: false,
        public_api: true,
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Verifies that a local type shadows a glob-imported variant in the value namespace,
/// while other variants remain glob-importable. This crate only defines those variants.
#[test]
fn enum_variant_overlapping_glob_of_enum_with_local_item() {
    get_test_data!(data, overlapping_glob_of_enum_with_local_item);
    let mut results: Vec<_> = collect_variant_importable_paths(&data);
    results.sort_unstable();

    let mut expected_results = vec![
        VariantImportablePath {
            name: "First".into(),
            path: vec![
                "overlapping_glob_of_enum_with_local_item".into(),
                "Foo".into(),
                "First".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Second".into(),
            path: vec![
                "overlapping_glob_of_enum_with_local_item".into(),
                "Foo".into(),
                "Second".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
        },
        VariantImportablePath {
            name: "Second".into(),
            path: vec![
                "overlapping_glob_of_enum_with_local_item".into(),
                "inner".into(),
                "Second".into(),
            ],
            doc_hidden: false,
            deprecated: false,
            public_api: true,
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
            name: "HiddenGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "BothHiddenAndVisible".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "BothHiddenAndVisibleSameName".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenSubmodulePathCanSortFirst".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "DuplicateGlobHiddenAndVisible".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "DuplicateGlobHiddenDeprecatedAndVisible".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "DuplicateGlobDeprecatedAndVisible".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "DeprecatedGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenDeprecatedGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "NestedHiddenGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "NestedHiddenDeprecatedGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "PlainGlobOnly".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenPerItemThenRootPerItem".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenPerItemThenRootGlob".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenGlobThenRootPerItem".into(),
            doc_hidden: false,
            deprecated: false,
            public_api_eligible: true,
        },
        Output {
            name: "HiddenGlobThenRootGlob".into(),
            doc_hidden: false,
            deprecated: false,
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

#[test]
fn importable_items_cover_expected_kinds() {
    get_test_data!(data, target_feature);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Importable {
                name @output
                kind: __typename @output
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
        kind: String,
    }

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "Example".into(),
            kind: "Struct".into(),
        },
        Output {
            name: "Trait".into(),
            kind: "Trait".into(),
        },
        Output {
            name: "globally_enabled_features_are_still_listed".into(),
            kind: "Function".into(),
        },
        Output {
            name: "implies_avx".into(),
            kind: "Function".into(),
        },
        Output {
            name: "multiple_attrs".into(),
            kind: "Function".into(),
        },
        Output {
            name: "multiple_enable_clauses".into(),
            kind: "Function".into(),
        },
        Output {
            name: "target_feature".into(),
            kind: "Module".into(),
        },
        Output {
            name: "top_level_fn".into(),
            kind: "Function".into(),
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            kind: "Function".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn importable_items_cover_more_kinds() {
    let query = r#"
{
    Crate {
        item {
            ... on Importable {
                name @output
                kind: __typename @output
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
        kind: String,
    }

    {
        get_test_data!(data, enum_discriminants);
        let adapter = RustdocAdapter::new(&data, None);
        let adapter = Arc::new(&adapter);

        let mut results: Vec<_> =
            trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
                .expect("failed to run enum discriminants importable query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.sort_unstable();

        let mut expected_results = vec![
            Output {
                name: "A".into(),
                kind: "Enum".into(),
            },
            Output {
                name: "Zero".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "One".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Two".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Three".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Four".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Five".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "FieldlessWithDiscrimants".into(),
                kind: "Enum".into(),
            },
            Output {
                name: "First".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Tuple".into(),
                kind: "TupleVariant".into(),
            },
            Output {
                name: "Second".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Struct".into(),
                kind: "StructVariant".into(),
            },
            Output {
                name: "Unit".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Fieldful".into(),
                kind: "Enum".into(),
            },
            Output {
                name: "Unit".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Tuple".into(),
                kind: "TupleVariant".into(),
            },
            Output {
                name: "Struct".into(),
                kind: "StructVariant".into(),
            },
            Output {
                name: "Unit2".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "FieldfulNoRepr".into(),
                kind: "Enum".into(),
            },
            Output {
                name: "Unit".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Tuple".into(),
                kind: "TupleVariant".into(),
            },
            Output {
                name: "Struct".into(),
                kind: "StructVariant".into(),
            },
            Output {
                name: "Pathological".into(),
                kind: "Enum".into(),
            },
            Output {
                name: "Min".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "MinPlusOne".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "MinPlusTwo".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "Max".into(),
                kind: "PlainVariant".into(),
            },
            Output {
                name: "enum_discriminants".into(),
                kind: "Module".into(),
            },
        ];
        expected_results.sort_unstable();

        similar_asserts::assert_eq!(expected_results, results);
    }

    {
        get_test_data!(data, unions);
        let adapter = RustdocAdapter::new(&data, None);
        let adapter = Arc::new(&adapter);

        let mut results: Vec<Output> =
            trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
                .expect("failed to run unions importable query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.retain(|row| {
            matches!(
                row.name.as_str(),
                "PublicImportable"
                    | "ModuleHidden"
                    | "DeprecatedModuleHidden"
                    | "Hidden"
                    | "ModuleDeprecated"
            )
        });
        results.sort_unstable();

        let mut expected_results = vec![
            Output {
                name: "PublicImportable".into(),
                kind: "Union".into(),
            },
            Output {
                name: "ModuleHidden".into(),
                kind: "Union".into(),
            },
            Output {
                name: "DeprecatedModuleHidden".into(),
                kind: "Union".into(),
            },
            Output {
                name: "Hidden".into(),
                kind: "Union".into(),
            },
            Output {
                name: "ModuleDeprecated".into(),
                kind: "Union".into(),
            },
        ];
        expected_results.sort_unstable();

        similar_asserts::assert_eq!(expected_results, results);
    }

    {
        get_test_data!(data, reexport_consts_and_statics);
        let adapter = RustdocAdapter::new(&data, None);
        let adapter = Arc::new(&adapter);

        let mut results: Vec<_> =
            trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
                .expect("failed to run consts and statics importable query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.sort_unstable();

        let mut expected_results = vec![
            Output {
                name: "FIRST".into(),
                kind: "Constant".into(),
            },
            Output {
                name: "SECOND".into(),
                kind: "Static".into(),
            },
            Output {
                name: "inner".into(),
                kind: "Module".into(),
            },
            Output {
                name: "reexport_consts_and_statics".into(),
                kind: "Module".into(),
            },
        ];
        expected_results.sort_unstable();

        similar_asserts::assert_eq!(expected_results, results);
    }

    {
        get_test_data!(data, declarative_macros);
        let adapter = RustdocAdapter::new(&data, None);
        let adapter = Arc::new(&adapter);

        let mut results: Vec<Output> =
            trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
                .expect("failed to run declarative macros importable query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.retain(|row| {
            matches!(
                row.name.as_str(),
                "top_level" | "nested_private" | "nested_public" | "hidden_parent" | "hidden"
            )
        });
        results.sort_unstable();

        let mut expected_results = vec![
            Output {
                name: "top_level".into(),
                kind: "Macro".into(),
            },
            Output {
                name: "hidden".into(),
                kind: "Module".into(),
            },
            Output {
                name: "nested_private".into(),
                kind: "Macro".into(),
            },
            Output {
                name: "nested_public".into(),
                kind: "Macro".into(),
            },
            Output {
                name: "hidden_parent".into(),
                kind: "Macro".into(),
            },
            Output {
                name: "hidden".into(),
                kind: "Macro".into(),
            },
        ];
        expected_results.sort_unstable();

        similar_asserts::assert_eq!(expected_results, results);
    }
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
fn defaulted_trait_items_overridden_in_impls() {
    get_test_data!(data, defaulted_trait_items_overridden_in_impls);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait_name"])
                    }

                    associated_constant @fold {
                        consts: name @output
                    }

                    method @fold {
                        methods: name @output
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, &str> = BTreeMap::default();
    variables.insert("trait_name", "Trait");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        consts: Vec<String>,
        methods: Vec<String>,
    }

    let mut expected_results = vec![Output {
        name: "Example".into(),
        consts: vec!["N".into()],
        methods: vec!["method".into()],
    }];
    expected_results.sort_unstable();

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
}

#[test]
fn defaulted_trait_items_overridden_in_impls_when_looked_up_by_name() {
    get_test_data!(data, defaulted_trait_items_overridden_in_impls);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @output

                impl {
                    method {
                        method: name @filter(op: "=", value: ["$method"]) @output
                    }
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, &str> = BTreeMap::default();
    variables.insert("method", "method");

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        method: String,
    }

    let mut expected_results = vec![
        // We *must* only get one result for `Example` here.
        //
        // If we get two, we've erroneously returned both
        // the trait's provided default impl for the method
        // and the `impl Trait for Example` override for the method.
        Output {
            name: "Example".into(),
            method: "method".into(),
        },
        // `SameNameExample` has a same-named associated type but no method override;
        // the provided method should still be discoverable.
        Output {
            name: "SameNameExample".into(),
            method: "method".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
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
    let top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();

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
            name: "impl_method".into(),
            generic_kind: "GenericLifetimeParameter".into(),
            generic_name: "'b".into(),
        },
        Output {
            name: "impl_method".into(),
            generic_kind: "GenericTypeParameter".into(),
            generic_name: "U".into(),
        },
        Output {
            name: "impl_method".into(),
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
    let top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();

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
            name: "impl_method".into(),
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
    let top_level_variables: BTreeMap<&str, &str> = BTreeMap::default();

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
        Output {
            name: "method".into(),
            generic_name: "M".into(),
            has_default: false,
        },
        Output {
            name: "impl_method".into(),
            generic_name: "M".into(),
            has_default: false,
        },
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
fn features_directly_enables() {
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
    struct FeatureEnablesOutput {
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
        FeatureEnablesOutput {
            name: "default".into(),
            enables: Some("foo".into()),
        },
        FeatureEnablesOutput {
            name: "default".into(),
            enables: Some("bar".into()),
        },
        FeatureEnablesOutput {
            name: "foo".into(),
            enables: Some("baz".into()),
        },
        FeatureEnablesOutput {
            name: "bar".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "baz".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "opt_in".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "serde".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "serde_json".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "nightly".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "chain_root".into(),
            enables: Some("chain_mid".into()),
        },
        FeatureEnablesOutput {
            name: "chain_mid".into(),
            enables: Some("chain_deep".into()),
        },
        FeatureEnablesOutput {
            name: "chain_deep".into(),
            enables: Some("chain_leaf".into()),
        },
        FeatureEnablesOutput {
            name: "chain_leaf".into(),
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
fn features_transitively_enables() {
    get_test_data!(data, features);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        feature {
            name @output

            transitively_enables @optional {
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
    struct FeatureEnablesOutput {
        name: String,
        enables: Option<String>,
    }

    let mut results: Vec<_> = trustfall::execute_query(&schema, adapter.clone(), query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        FeatureEnablesOutput {
            name: "default".into(),
            enables: Some("foo".into()),
        },
        FeatureEnablesOutput {
            name: "default".into(),
            enables: Some("bar".into()),
        },
        FeatureEnablesOutput {
            name: "default".into(),
            enables: Some("baz".into()),
        },
        FeatureEnablesOutput {
            name: "foo".into(),
            enables: Some("baz".into()),
        },
        FeatureEnablesOutput {
            name: "bar".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "baz".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "opt_in".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "serde".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "serde_json".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "nightly".into(),
            enables: None,
        },
        FeatureEnablesOutput {
            name: "chain_root".into(),
            enables: Some("chain_mid".into()),
        },
        FeatureEnablesOutput {
            name: "chain_root".into(),
            enables: Some("chain_deep".into()),
        },
        FeatureEnablesOutput {
            name: "chain_root".into(),
            enables: Some("chain_leaf".into()),
        },
        FeatureEnablesOutput {
            name: "chain_mid".into(),
            enables: Some("chain_deep".into()),
        },
        FeatureEnablesOutput {
            name: "chain_mid".into(),
            enables: Some("chain_leaf".into()),
        },
        FeatureEnablesOutput {
            name: "chain_deep".into(),
            enables: Some("chain_leaf".into()),
        },
        FeatureEnablesOutput {
            name: "chain_leaf".into(),
            enables: None,
        },
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
            bound: "core::fmt::Debug".into(),
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
fn rust_std_function_facets_report_stable_guarantee() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @filter(op: "one_of", value: ["$names"]) @output
                const @output
                has_body @output
                public_api_eligible @output
                signature @output
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "names" => FieldValue::List(vec![
            FieldValue::String("stable_const_stable".into()),
            FieldValue::String("stable_const_unstable".into()),
            FieldValue::String("unstable_const_function".into()),
        ].into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        #[serde(rename = "const")]
        const_: bool,
        has_body: bool,
        public_api_eligible: bool,
        signature: String,
    }

    let mut results: Vec<_> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "stable_const_stable".into(),
            const_: true,
            has_body: true,
            public_api_eligible: true,
            signature: "const fn stable_const_stable() -> u32".into(),
        },
        Output {
            name: "stable_const_unstable".into(),
            const_: false,
            has_body: true,
            public_api_eligible: true,
            signature: "fn stable_const_unstable() -> u32".into(),
        },
        Output {
            name: "unstable_const_function".into(),
            const_: true,
            has_body: true,
            public_api_eligible: false,
            signature: "const fn unstable_const_function() -> u32".into(),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn default_policy_ignores_rust_std_const_stability() {
    let rustdoc = crate::test_util::load_pregenerated_rustdoc("rust_std_stability");
    let data = PackageIndex::from_crate(&rustdoc);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @filter(op: "=", value: ["$name"])
                const @output
                signature @output
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "name" => FieldValue::String("stable_const_unstable".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        #[serde(rename = "const")]
        const_: bool,
        signature: String,
    }

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    similar_asserts::assert_eq!(
        vec![Output {
            const_: true,
            signature: "const fn stable_const_unstable() -> u32".into(),
        }],
        results,
    );
}

#[test]
fn rust_std_default_facets_report_stable_guarantee() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    let trait_name = FieldValue::String("DefaultStabilityTrait".into());

    let method_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                method {
                    name @filter(op: "one_of", value: ["$methods"]) @output
                    has_body @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name.clone(),
        "methods" => FieldValue::List(vec![
            FieldValue::String("stable_default_method".into()),
            FieldValue::String("unstable_default_method".into()),
            FieldValue::String("required_method".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct MethodOutput {
        name: String,
        has_body: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<MethodOutput> =
        trustfall::execute_query(&schema, adapter.clone(), method_query, variables)
            .expect("failed to run method query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        MethodOutput {
            name: "stable_default_method".into(),
            has_body: true,
            public_api_eligible: true,
        },
        MethodOutput {
            name: "unstable_default_method".into(),
            has_body: false,
            public_api_eligible: true,
        },
        MethodOutput {
            name: "required_method".into(),
            has_body: false,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let associated_type_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                associated_type {
                    name @filter(op: "one_of", value: ["$types"]) @output
                    has_default @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name.clone(),
        "types" => FieldValue::List(vec![
            FieldValue::String("StableDefaultType".into()),
            FieldValue::String("UnstableDefaultType".into()),
            FieldValue::String("RequiredType".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct AssociatedTypeOutput {
        name: String,
        has_default: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<AssociatedTypeOutput> =
        trustfall::execute_query(&schema, adapter.clone(), associated_type_query, variables)
            .expect("failed to run associated type query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        AssociatedTypeOutput {
            name: "StableDefaultType".into(),
            has_default: true,
            public_api_eligible: true,
        },
        AssociatedTypeOutput {
            name: "UnstableDefaultType".into(),
            has_default: false,
            public_api_eligible: true,
        },
        AssociatedTypeOutput {
            name: "RequiredType".into(),
            has_default: false,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let associated_const_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                associated_constant {
                    name @filter(op: "one_of", value: ["$consts"]) @output
                    default @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name,
        "consts" => FieldValue::List(vec![
            FieldValue::String("STABLE_DEFAULT_CONST".into()),
            FieldValue::String("UNSTABLE_DEFAULT_CONST".into()),
            FieldValue::String("REQUIRED_CONST".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct AssociatedConstOutput {
        name: String,
        default: Option<String>,
        public_api_eligible: bool,
    }

    let mut results: Vec<AssociatedConstOutput> =
        trustfall::execute_query(&schema, adapter, associated_const_query, variables)
            .expect("failed to run associated const query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        AssociatedConstOutput {
            name: "STABLE_DEFAULT_CONST".into(),
            default: Some("1".into()),
            public_api_eligible: true,
        },
        AssociatedConstOutput {
            name: "UNSTABLE_DEFAULT_CONST".into(),
            default: None,
            public_api_eligible: true,
        },
        AssociatedConstOutput {
            name: "REQUIRED_CONST".into(),
            default: None,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn default_policy_ignores_rust_std_default_stability() {
    let rustdoc = crate::test_util::load_pregenerated_rustdoc("rust_std_stability");
    let data = PackageIndex::from_crate(&rustdoc);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    let trait_name = FieldValue::String("DefaultStabilityTrait".into());

    let method_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                method {
                    name @filter(op: "one_of", value: ["$methods"]) @output
                    has_body @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name.clone(),
        "methods" => FieldValue::List(vec![
            FieldValue::String("stable_default_method".into()),
            FieldValue::String("unstable_default_method".into()),
            FieldValue::String("required_method".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct MethodOutput {
        name: String,
        has_body: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<MethodOutput> =
        trustfall::execute_query(&schema, adapter.clone(), method_query, variables)
            .expect("failed to run method query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        MethodOutput {
            name: "stable_default_method".into(),
            has_body: true,
            public_api_eligible: true,
        },
        MethodOutput {
            name: "unstable_default_method".into(),
            has_body: true,
            public_api_eligible: true,
        },
        MethodOutput {
            name: "required_method".into(),
            has_body: false,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let associated_type_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                associated_type {
                    name @filter(op: "one_of", value: ["$types"]) @output
                    has_default @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name.clone(),
        "types" => FieldValue::List(vec![
            FieldValue::String("StableDefaultType".into()),
            FieldValue::String("UnstableDefaultType".into()),
            FieldValue::String("RequiredType".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct AssociatedTypeOutput {
        name: String,
        has_default: bool,
        public_api_eligible: bool,
    }

    let mut results: Vec<AssociatedTypeOutput> =
        trustfall::execute_query(&schema, adapter.clone(), associated_type_query, variables)
            .expect("failed to run associated type query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        AssociatedTypeOutput {
            name: "StableDefaultType".into(),
            has_default: true,
            public_api_eligible: true,
        },
        AssociatedTypeOutput {
            name: "UnstableDefaultType".into(),
            has_default: true,
            public_api_eligible: true,
        },
        AssociatedTypeOutput {
            name: "RequiredType".into(),
            has_default: false,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);

    let associated_const_query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "=", value: ["$trait"])

                associated_constant {
                    name @filter(op: "one_of", value: ["$consts"]) @output
                    default @output
                    public_api_eligible @output
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "trait" => trait_name,
        "consts" => FieldValue::List(vec![
            FieldValue::String("STABLE_DEFAULT_CONST".into()),
            FieldValue::String("UNSTABLE_DEFAULT_CONST".into()),
            FieldValue::String("REQUIRED_CONST".into()),
        ].into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct AssociatedConstOutput {
        name: String,
        default: Option<String>,
        public_api_eligible: bool,
    }

    let mut results: Vec<AssociatedConstOutput> =
        trustfall::execute_query(&schema, adapter, associated_const_query, variables)
            .expect("failed to run associated const query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        AssociatedConstOutput {
            name: "STABLE_DEFAULT_CONST".into(),
            default: Some("1".into()),
            public_api_eligible: true,
        },
        AssociatedConstOutput {
            name: "UNSTABLE_DEFAULT_CONST".into(),
            default: Some("2".into()),
            public_api_eligible: true,
        },
        AssociatedConstOutput {
            name: "REQUIRED_CONST".into(),
            default: None,
            public_api_eligible: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn rust_std_impl_methods_hide_omitted_unstable_defaults() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    // This query has no `method.name` filter, so both `ImplOwner.impl`
    // and nested `Impl.method` use their unfiltered paths.
    // It checks the complete std-mode method list for an impl that
    // omits an unstable default and one that overrides it.
    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "one_of", value: ["$owners"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait"])
                    }

                    method {
                        method_name: name @output
                        has_body @output
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "owners" => FieldValue::List(vec![
            FieldValue::String("DefaultStabilityImplOmittingDefaults".into()),
            FieldValue::String("DefaultStabilityImplOverridingDefault".into()),
        ].into()),
        "trait" => FieldValue::String("DefaultStabilityTrait".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        method_name: String,
        has_body: bool,
    }

    let mut results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            owner: "DefaultStabilityImplOmittingDefaults".into(),
            method_name: "required_method".into(),
            has_body: true,
        },
        Output {
            owner: "DefaultStabilityImplOmittingDefaults".into(),
            method_name: "stable_default_method".into(),
            has_body: true,
        },
        Output {
            owner: "DefaultStabilityImplOverridingDefault".into(),
            method_name: "required_method".into(),
            has_body: true,
        },
        Output {
            owner: "DefaultStabilityImplOverridingDefault".into(),
            method_name: "stable_default_method".into(),
            has_body: true,
        },
        Output {
            owner: "DefaultStabilityImplOverridingDefault".into(),
            method_name: "unstable_default_method".into(),
            has_body: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn rust_std_impl_method_lookup_hides_omitted_unstable_default() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    // This uses a mandatory `method.name` filter, so the owner-side
    // `ImplOwner.impl` method-name optimization may discard impls whose
    // matching method is absent before the nested `Impl.method` resolver runs.
    // Under std mode, the impl that omits the unstable default should disappear.
    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "one_of", value: ["$owners"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait"])
                    }

                    method {
                        method_name: name @filter(op: "=", value: ["$method"]) @output
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "owners" => FieldValue::List(vec![
            FieldValue::String("DefaultStabilityImplOmittingDefaults".into()),
            FieldValue::String("DefaultStabilityImplOverridingDefault".into()),
        ].into()),
        "trait" => FieldValue::String("DefaultStabilityTrait".into()),
        "method" => FieldValue::String("unstable_default_method".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        method_name: String,
    }

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    similar_asserts::assert_eq!(
        vec![Output {
            owner: "DefaultStabilityImplOverridingDefault".into(),
            method_name: "unstable_default_method".into(),
        }],
        results,
    );
}

#[test]
fn default_policy_impl_method_lookup_includes_omitted_unstable_default() {
    let rustdoc = crate::test_util::load_pregenerated_rustdoc("rust_std_stability");
    let data = PackageIndex::from_crate(&rustdoc);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    // This is the same owner-side method-name optimization shape as
    // the std-policy test above, but under the default indexing policy.
    // Ordinary crates should ignore std default-body stability and keep the
    // omitted unstable default visible.
    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "one_of", value: ["$owners"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait"])
                    }

                    method {
                        method_name: name @filter(op: "=", value: ["$method"]) @output
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "owners" => FieldValue::List(vec![
            FieldValue::String("DefaultStabilityImplOmittingDefaults".into()),
            FieldValue::String("DefaultStabilityImplOverridingDefault".into()),
        ].into()),
        "trait" => FieldValue::String("DefaultStabilityTrait".into()),
        "method" => FieldValue::String("unstable_default_method".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        method_name: String,
    }

    let mut results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            owner: "DefaultStabilityImplOmittingDefaults".into(),
            method_name: "unstable_default_method".into(),
        },
        Output {
            owner: "DefaultStabilityImplOverridingDefault".into(),
            method_name: "unstable_default_method".into(),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn impl_method_name_lookup_applies_default_body_stability_policy() {
    let rustdoc = crate::test_util::load_pregenerated_rustdoc("rust_std_stability");
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    // The folded `method` edge keeps impls whose matching-method count is zero,
    // so the owner-side `ImplOwner.impl` method-name optimization cannot remove
    // those impls before the nested `Impl.method` resolver runs.
    // This pins down the behavior of the name-filtered `Impl.method` path itself:
    // std mode hides omitted unstable defaults, while default mode keeps them visible.
    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "one_of", value: ["$owners"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait"])
                    }

                    method @fold @transform(op: "count") @output(name: "matching_methods") {
                        name @filter(op: "=", value: ["$method"])
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "owners" => FieldValue::List(vec![
            FieldValue::String("DefaultStabilityImplOmittingDefaults".into()),
            FieldValue::String("DefaultStabilityImplOverridingDefault".into()),
        ].into()),
        "trait" => FieldValue::String("DefaultStabilityTrait".into()),
        "method" => FieldValue::String("unstable_default_method".into()),
    };

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        matching_methods: u64,
    }

    let data = PackageIndex::from_rust_std_component_crate(&rustdoc);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);
    let mut rust_std_results: Vec<Output> =
        trustfall::execute_query(&schema, adapter, query, variables.clone())
            .expect("failed to run std policy query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    rust_std_results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                owner: "DefaultStabilityImplOmittingDefaults".into(),
                matching_methods: 0,
            },
            Output {
                owner: "DefaultStabilityImplOverridingDefault".into(),
                matching_methods: 1,
            },
        ],
        rust_std_results,
    );

    let data = PackageIndex::from_crate(&rustdoc);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);
    let mut default_results: Vec<Output> =
        trustfall::execute_query(&schema, adapter, query, variables)
            .expect("failed to run default policy query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    default_results.sort_unstable();

    similar_asserts::assert_eq!(
        vec![
            Output {
                owner: "DefaultStabilityImplOmittingDefaults".into(),
                matching_methods: 1,
            },
            Output {
                owner: "DefaultStabilityImplOverridingDefault".into(),
                matching_methods: 1,
            },
        ],
        default_results,
    );
}

#[test]
fn rust_std_unstable_defaults_affect_public_api_sealing() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                name @filter(op: "one_of", value: ["$traits"]) @output
                unconditionally_sealed @output
                public_api_sealed @output
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "traits" => FieldValue::List(vec![
            FieldValue::String("StableHiddenDefaultIsNotSealed".into()),
            FieldValue::String("UnstableHiddenMethodDefaultIsPublicApiSealed".into()),
            FieldValue::String("UnstableHiddenAssocConstDefaultIsPublicApiSealed".into()),
            FieldValue::String("UnstableHiddenAssocTypeDefaultIsPublicApiSealed".into()),
        ].into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        unconditionally_sealed: bool,
        public_api_sealed: bool,
    }

    let mut results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "StableHiddenDefaultIsNotSealed".into(),
            unconditionally_sealed: false,
            public_api_sealed: false,
        },
        Output {
            name: "UnstableHiddenMethodDefaultIsPublicApiSealed".into(),
            unconditionally_sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "UnstableHiddenAssocConstDefaultIsPublicApiSealed".into(),
            unconditionally_sealed: false,
            public_api_sealed: true,
        },
        Output {
            name: "UnstableHiddenAssocTypeDefaultIsPublicApiSealed".into(),
            unconditionally_sealed: false,
            public_api_sealed: true,
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn rust_std_const_impl_methods_use_inherited_const_stability() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                name @filter(op: "=", value: ["$owner"])

                impl {
                    method {
                        name @filter(op: "=", value: ["$method"])
                        const @output
                        signature @output
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "owner" => FieldValue::String("ConstImplOwner".into()),
        "method" => FieldValue::String("const_impl_method".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        #[serde(rename = "const")]
        const_: bool,
        signature: String,
    }

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    similar_asserts::assert_eq!(
        vec![Output {
            const_: false,
            signature: "fn const_impl_method() -> u32".into(),
        }],
        results,
    );
}

#[test]
fn rust_std_signatures_hide_const_trait_markers() {
    get_rust_std_test_data!(data, rust_std_stability);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @filter(op: "=", value: ["$name"])
                signature @output(name: "function_signature")

                parameter {
                    normalized_type_signature {
                        signature @output(name: "parameter_signature")
                    }
                }

                return_value {
                    normalized_type_signature {
                        signature @output(name: "return_signature")
                    }
                }
            }
        }
    }
}
    "#;
    let variables = btreemap! {
        "name" => FieldValue::String("const_trait_marker".into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        function_signature: String,
        parameter_signature: String,
        return_signature: String,
    }

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    let expected_results = vec![Output {
        function_signature:
            "fn const_trait_marker(arg: impl FixtureConstBound) -> impl FixtureConstBound".into(),
        parameter_signature: "IT1_1".into(),
        return_signature: "impl ::rust_std_stability::FixtureConstBound".into(),
    }];

    similar_asserts::assert_eq!(expected_results, results);
}

/// `const Trait` isn't stable yet, so don't show the const impls in the normalization.
#[test]
fn function_return_normalized_type_signatures_hide_const_trait_markers_before_sorting() {
    get_test_data!(data, maybe_const_function_signature);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                return_value {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, FieldValue> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut expected_results = vec![
        Output {
            name: "maybe_const_function_signature".into(),
            signature: "impl ::maybe_const_function_signature::MaybeConst".into(),
        },
        Output {
            name: "maybe_const_return_bound_const_then_plain".into(),
            signature: "impl ::maybe_const_function_signature::MaybeConst + ::maybe_const_function_signature::Plain".into(),
        },
        Output {
            name: "maybe_const_return_bound_plain_then_const".into(),
            signature: "impl ::maybe_const_function_signature::MaybeConst + ::maybe_const_function_signature::Plain".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> = trustfall::execute_query(&schema, adapter, query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

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
fn item_lookup_by_multiple_importable_paths_visits_each_item_once() {
    get_test_data!(data, item_lookup_optimization);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                name @output

                importable_path {
                    path @filter(op: "one_of", value: ["$paths"]) @output
                }
            }
        }
    }
}
    "#;

    let variables = btreemap! {
        "paths" => FieldValue::List(vec![
            FieldValue::List(vec![
                FieldValue::String("item_lookup_optimization".into()),
                FieldValue::String("MultiPathItem".into()),
            ].into()),
            FieldValue::List(vec![
                FieldValue::String("item_lookup_optimization".into()),
                FieldValue::String("inner".into()),
                FieldValue::String("MultiPathItem".into()),
            ].into()),
        ].into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        path: Vec<String>,
    }

    let mut results: Vec<_> = trustfall::execute_query(&schema, adapter.clone(), query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "MultiPathItem".into(),
            path: vec!["item_lookup_optimization".into(), "MultiPathItem".into()],
        },
        Output {
            name: "MultiPathItem".into(),
            path: vec![
                "item_lookup_optimization".into(),
                "inner".into(),
                "MultiPathItem".into(),
            ],
        },
    ];
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
fn impl_lookup_by_multiple_method_names_visits_each_impl_once() {
    get_test_data!(data, method_lookup_optimization);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "=", value: ["$owner"]) @output

                inherent_impl {
                    method {
                        method: name @filter(op: "one_of", value: ["$methods"]) @output
                    }
                }
            }
        }
    }
}
    "#;

    let variables = btreemap! {
        "owner" => FieldValue::String("MultiMethodOwner".into()),
        "methods" => FieldValue::List(vec![
            FieldValue::String("first".into()),
            FieldValue::String("second".into()),
        ].into()),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        method: String,
    }

    let mut results: Vec<_> = trustfall::execute_query(&schema, adapter.clone(), query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            owner: "MultiMethodOwner".into(),
            method: "first".into(),
        },
        Output {
            owner: "MultiMethodOwner".into(),
            method: "second".into(),
        },
    ];
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
            position: Some(3),
        },
        Output {
            item_kind: "Function".into(),
            item: "function".into(),
            name: "M".into(),
            kind: "GenericConstParameter".into(),
            position: Some(4),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "'a".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "'b".into(),
            kind: "GenericLifetimeParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "T".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(1),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "N".into(),
            kind: "GenericConstParameter".into(),
            position: Some(2),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "U".into(),
            kind: "GenericTypeParameter".into(),
            position: Some(3),
        },
        Output {
            item_kind: "Function".into(),
            item: "mixed_order".into(),
            name: "M".into(),
            kind: "GenericConstParameter".into(),
            position: Some(4),
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
            position: Some(2),
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
            position: Some(3),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_variant_positions() {
    get_test_data!(data, enum_variants_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output
                    variant {
                        variant_name: name @output
                        variant_position: position @output
                        variant_typename: __typename @output
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
        enum_name: String,
        variant_name: String,
        variant_position: i64,
        variant_typename: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "First".into(),
            variant_position: 1,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Second".into(),
            variant_position: 2,
            variant_typename: "TupleVariant".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Third".into(),
            variant_position: 3,
            variant_typename: "StructVariant".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Fourth".into(),
            variant_position: 4,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Fifth".into(),
            variant_position: 5,
            variant_typename: "TupleVariant".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Sixth".into(),
            variant_position: 6,
            variant_typename: "StructVariant".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "A".into(),
            variant_position: 1,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "B".into(),
            variant_position: 2,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "C".into(),
            variant_position: 3,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "D".into(),
            variant_position: 4,
            variant_typename: "PlainVariant".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "E".into(),
            variant_position: 5,
            variant_typename: "PlainVariant".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_variant_name_resolution_dynamic() {
    get_test_data!(data, enum_variants_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output

                    variant {
                        variant_name: name @tag @output
                    }

                    variant {
                        other_name: name @filter(op: "=", value: ["%variant_name"]) @output
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
        enum_name: String,
        variant_name: String,
        other_name: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "First".into(),
            other_name: "First".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Second".into(),
            other_name: "Second".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Third".into(),
            other_name: "Third".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Fourth".into(),
            other_name: "Fourth".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Fifth".into(),
            other_name: "Fifth".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Sixth".into(),
            other_name: "Sixth".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "A".into(),
            other_name: "A".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "B".into(),
            other_name: "B".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "C".into(),
            other_name: "C".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "D".into(),
            other_name: "D".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            variant_name: "E".into(),
            other_name: "E".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_variant_name_resolution_static() {
    get_test_data!(data, enum_variants_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output

                    variant {
                        name: name @filter(op: "one_of", value: ["$name"]) @output
                    }
                }
            }
        }
    }
    "#;

    let variables: BTreeMap<&str, Vec<&str>> =
        BTreeMap::from([("name", ["First", "Second", "Third", "A"].to_vec())]);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        enum_name: String,
        name: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            enum_name: "AllVariantTypes".into(),
            name: "First".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            name: "Second".into(),
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            name: "Third".into(),
        },
        Output {
            enum_name: "WithDiscriminants".into(),
            name: "A".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_struct_variant_fields() {
    get_test_data!(data, enum_variants_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output
                    variant {
                        ... on StructVariant {
                            variant_name: name @output
                            field {
                                struct_field_name: name @output
                                struct_field_position: position @output
                            }
                        }
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
        enum_name: String,
        variant_name: String,
        struct_field_name: String,
        struct_field_position: i64,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Third".into(),
            struct_field_name: "x".into(),
            struct_field_position: 1,
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Third".into(),
            struct_field_name: "y".into(),
            struct_field_position: 2,
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Sixth".into(),
            struct_field_name: "name".into(),
            struct_field_position: 1,
        },
    ];

    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_tuple_variant_fields() {
    get_test_data!(data, enum_variants_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output
                    variant {
                        ... on TupleVariant {
                            variant_name: name @output
                            field {
                                tuple_field_name: name @output
                                tuple_field_position: position @output
                            }
                        }
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
        enum_name: String,
        variant_name: String,
        tuple_field_name: String,
        tuple_field_position: i64,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Second".into(),
            tuple_field_name: "0".into(),
            tuple_field_position: 1,
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Second".into(),
            tuple_field_name: "1".into(),
            tuple_field_position: 2,
        },
        Output {
            enum_name: "AllVariantTypes".into(),
            variant_name: "Fifth".into(),
            tuple_field_name: "0".into(),
            tuple_field_position: 1,
        },
    ];

    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn struct_field_positions() {
    get_test_data!(data, struct_fields_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Struct {
                    struct_name: name @output
                    struct_type @output
                    field {
                        field_name: name @output
                        field_position: position @output
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
        struct_name: String,
        struct_type: String,
        field_name: String,
        field_position: i64,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            struct_name: "PlainStruct".into(),
            struct_type: "plain".into(),
            field_name: "first".into(),
            field_position: 1,
        },
        Output {
            struct_name: "PlainStruct".into(),
            struct_type: "plain".into(),
            field_name: "second".into(),
            field_position: 2,
        },
        Output {
            struct_name: "PlainStruct".into(),
            struct_type: "plain".into(),
            field_name: "third".into(),
            field_position: 3,
        },
        Output {
            struct_name: "TupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "0".into(),
            field_position: 1,
        },
        Output {
            struct_name: "TupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "1".into(),
            field_position: 2,
        },
        Output {
            struct_name: "TupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "2".into(),
            field_position: 3,
        },
        Output {
            struct_name: "ReprCStruct".into(),
            struct_type: "plain".into(),
            field_name: "a".into(),
            field_position: 1,
        },
        Output {
            struct_name: "ReprCStruct".into(),
            struct_type: "plain".into(),
            field_name: "b".into(),
            field_position: 2,
        },
        Output {
            struct_name: "ReprCStruct".into(),
            struct_type: "plain".into(),
            field_name: "c".into(),
            field_position: 3,
        },
        Output {
            struct_name: "ReprPackedStruct".into(),
            struct_type: "plain".into(),
            field_name: "x".into(),
            field_position: 1,
        },
        Output {
            struct_name: "ReprPackedStruct".into(),
            struct_type: "plain".into(),
            field_name: "y".into(),
            field_position: 2,
        },
        Output {
            struct_name: "ReprPackedStruct".into(),
            struct_type: "plain".into(),
            field_name: "z".into(),
            field_position: 3,
        },
        Output {
            struct_name: "ReprPackedWithAlignment".into(),
            struct_type: "plain".into(),
            field_name: "x".into(),
            field_position: 1,
        },
        Output {
            struct_name: "ReprPackedWithAlignment".into(),
            struct_type: "plain".into(),
            field_name: "y".into(),
            field_position: 2,
        },
        Output {
            struct_name: "ReprPackedWithAlignment".into(),
            struct_type: "plain".into(),
            field_name: "z".into(),
            field_position: 3,
        },
        Output {
            struct_name: "ReprCTupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "0".into(),
            field_position: 1,
        },
        Output {
            struct_name: "ReprCTupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "1".into(),
            field_position: 2,
        },
        Output {
            struct_name: "ReprCTupleStruct".into(),
            struct_type: "tuple".into(),
            field_name: "2".into(),
            field_position: 3,
        },
        Output {
            struct_name: "ReprTransparentStruct".into(),
            struct_type: "plain".into(),
            field_name: "inner".into(),
            field_position: 1,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn union_field_positions() {
    get_test_data!(data, union_fields_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Union {
                    union_name: name @output
                    field {
                        field_name: name @output
                        field_position: position @output
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
        union_name: String,
        field_name: String,
        field_position: i64,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            union_name: "SimpleUnion".into(),
            field_name: "first".into(),
            field_position: 1,
        },
        Output {
            union_name: "SimpleUnion".into(),
            field_name: "second".into(),
            field_position: 2,
        },
        Output {
            union_name: "SimpleUnion".into(),
            field_name: "third".into(),
            field_position: 3,
        },
        Output {
            union_name: "UnionWithDifferentSizes".into(),
            field_name: "small".into(),
            field_position: 1,
        },
        Output {
            union_name: "UnionWithDifferentSizes".into(),
            field_name: "medium".into(),
            field_position: 2,
        },
        Output {
            union_name: "UnionWithDifferentSizes".into(),
            field_name: "large".into(),
            field_position: 3,
        },
        Output {
            union_name: "UnionWithCompoundTypes".into(),
            field_name: "int_array".into(),
            field_position: 1,
        },
        Output {
            union_name: "UnionWithCompoundTypes".into(),
            field_name: "float_array".into(),
            field_position: 2,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn non_exhaustive_attribute() {
    get_test_data!(data, non_exhaustive_attribute);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let enum_query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    enum_name: name @output
                    enum_attrs: attrs @output

                    enum_attr_: attribute @optional {
                        raw: raw_attribute @output
                        content {
                            base @output
                        }
                    }

                    variant {
                        variant: name @output
                        variant_attrs: attrs @output

                        variant_attr_: attribute @optional {
                            raw: raw_attribute @output
                            content {
                                base @output
                            }
                        }
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
    struct EnumOutput {
        enum_name: String,
        enum_attrs: Vec<String>,
        enum_attr_raw: Option<String>,
        enum_attr_base: Option<String>,
        variant: String,
        variant_attrs: Vec<String>,
        variant_attr_raw: Option<String>,
        variant_attr_base: Option<String>,
    }

    let mut results: Vec<EnumOutput> =
        trustfall::execute_query(&schema, adapter.clone(), enum_query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        EnumOutput {
            enum_name: "NonExhaustiveEnum".into(),
            enum_attrs: vec!["#[non_exhaustive]".into()],
            enum_attr_raw: Some("#[non_exhaustive]".into()),
            enum_attr_base: Some("non_exhaustive".into()),
            variant: "First".into(),
            variant_attrs: vec![],
            variant_attr_raw: None,
            variant_attr_base: None,
        },
        EnumOutput {
            enum_name: "MyEnum".into(),
            enum_attrs: vec![],
            enum_attr_raw: None,
            enum_attr_base: None,
            variant: "NonExhaustiveVariant".into(),
            variant_attrs: vec!["#[non_exhaustive]".into()],
            variant_attr_raw: Some("#[non_exhaustive]".into()),
            variant_attr_base: Some("non_exhaustive".into()),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    let struct_query = r#"
    {
        Crate {
            item {
                ... on Struct {
                    name @output
                    attrs @output

                    attr_: attribute @optional {
                        raw: raw_attribute @output
                        content {
                            base @output
                        }
                    }
                }
            }
        }
    }
    "#;

    let variables: BTreeMap<&str, &str> = BTreeMap::new();

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        attrs: Vec<String>,
        attr_raw: Option<String>,
        attr_base: Option<String>,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), struct_query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![Output {
        name: "NonExhaustive".into(),
        attrs: vec!["#[non_exhaustive]".into()],
        attr_raw: Some("#[non_exhaustive]".into()),
        attr_base: Some("non_exhaustive".into()),
    }];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Ensure that `#[derive(...)]` applies the `#[automatically_derived]` attribute,
/// since that's key for the correct behavior of some of our queries.
#[test]
fn automatically_derived() {
    get_test_data!(data, automatically_derived);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Struct {
                    impl {
                        attrs @output

                        implemented_trait {
                            instantiated_name @output
                            bare_name @filter(op: "one_of", value: ["$traits"])

                            trait {
                                canonical_path {
                                    canonical_path: path @output
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    "#;

    let variables: BTreeMap<&str, Vec<&str>> = btreemap! {
        "traits" => vec!["Debug", "Clone", "PartialOrd", "Ord", "PartialEq", "Eq", "Hash"],
    };
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        instantiated_name: String,
        attrs: Vec<String>,
        canonical_path: Vec<String>,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            instantiated_name: "Clone".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "clone".into(), "Clone".into()],
        },
        Output {
            instantiated_name: "Debug".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "fmt".into(), "Debug".into()],
        },
        Output {
            instantiated_name: "Eq".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "cmp".into(), "Eq".into()],
        },
        Output {
            instantiated_name: "Hash".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "hash".into(), "Hash".into()],
        },
        Output {
            instantiated_name: "Ord".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "cmp".into(), "Ord".into()],
        },
        Output {
            instantiated_name: "PartialEq".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "cmp".into(), "PartialEq".into()],
        },
        Output {
            instantiated_name: "PartialOrd".into(),
            attrs: vec!["#[automatically_derived]".into()],
            canonical_path: vec!["core".into(), "cmp".into(), "PartialOrd".into()],
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn struct_repr_attributes() {
    get_test_data!(data, struct_fields_position);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Struct {
                    name @output
                    attrs @output

                    attr_: attribute {
                        raw: raw_attribute @output
                        content {
                            base @filter(op: "=", value: ["$repr"])
                            argument {
                                repr_kind: base @output
                            }
                        }
                    }
                }
            }
        }
    }
    "#;

    let variables: BTreeMap<&str, &str> = btreemap! {
        "repr" => "repr"
    };
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        attrs: Vec<String>,
        attr_raw: String,
        attr_repr_kind: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "ReprCStruct".into(),
            attrs: vec!["#[repr(C)]".into()],
            attr_raw: "#[repr(C)]".into(),
            attr_repr_kind: "C".into(),
        },
        Output {
            name: "ReprPackedStruct".into(),
            attrs: vec!["#[repr(packed(1))]".into()],
            attr_raw: "#[repr(packed(1))]".into(),
            attr_repr_kind: "packed".into(),
        },
        Output {
            name: "ReprPackedWithAlignment".into(),
            attrs: vec!["#[repr(packed(2))]".into()],
            attr_raw: "#[repr(packed(2))]".into(),
            attr_repr_kind: "packed".into(),
        },
        Output {
            name: "ReprCTupleStruct".into(),
            attrs: vec!["#[repr(C)]".into()],
            attr_raw: "#[repr(C)]".into(),
            attr_repr_kind: "C".into(),
        },
        Output {
            name: "ReprTransparentStruct".into(),
            attrs: vec!["#[repr(transparent)]".into()],
            attr_raw: "#[repr(transparent)]".into(),
            attr_repr_kind: "transparent".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn enum_repr_attributes() {
    get_test_data!(data, enum_discriminants);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Enum {
                    name @output
                    attrs @output

                    attr_: attribute {
                        raw: raw_attribute @output
                        content {
                            base @filter(op: "=", value: ["$repr"])
                            argument {
                                repr_kind: base @output
                            }
                        }
                    }
                }
            }
        }
    }
    "#;

    let variables: BTreeMap<&str, &str> = btreemap! {
        "repr" => "repr"
    };
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        attrs: Vec<String>,
        attr_raw: String,
        attr_repr_kind: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            name: "A".into(),
            attrs: vec!["#[repr(C)]".into()],
            attr_raw: "#[repr(C)]".into(),
            attr_repr_kind: "C".into(),
        },
        Output {
            name: "FieldlessWithDiscrimants".into(),
            attrs: vec!["#[repr(u8)]".into()],
            attr_raw: "#[repr(u8)]".into(),
            attr_repr_kind: "u8".into(),
        },
        Output {
            name: "Fieldful".into(),
            attrs: vec!["#[repr(C, i64)]".into()],
            attr_raw: "#[repr(C, i64)]".into(),
            attr_repr_kind: "C".into(),
        },
        Output {
            name: "Fieldful".into(),
            attrs: vec!["#[repr(C, i64)]".into()],
            attr_raw: "#[repr(C, i64)]".into(),
            attr_repr_kind: "i64".into(),
        },
        Output {
            name: "Pathological".into(),
            attrs: vec!["#[repr(i128)]".into()],
            attr_raw: "#[repr(i128)]".into(),
            attr_repr_kind: "i128".into(),
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn method_self_receiver() {
    get_test_data!(data, method_self_receivers);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
    {
        Crate {
            item {
                ... on Struct {
                    struct_name: name @output
                    attrs @output

                    inherent_impl {
                        method {
                            method_name: name @output
                            receiver {
                                by_value @output
                                by_reference @output
                                by_mut_reference @output
                                kind @output
                            }
                        }
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
        struct_name: String,
        method_name: String,
        by_value: bool,
        by_reference: bool,
        by_mut_reference: bool,
        kind: String,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    let mut expected_results = vec![
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Self".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Self".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Self".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Self".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_pinned_mut_ref".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Pin<&mut Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref_pinned_mut_ref".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Pin<&mut Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref_pinned_mut_ref_lifetime".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Pin<&mut Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref_pinned_mut_ref".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Pin<&mut Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref_pinned_mut_ref_lifetime".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Pin<&mut Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_boxed_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Box<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref_boxed_value".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Box<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref_boxed_value".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Box<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_rc_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Rc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref_rc_value".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Rc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref_rc_value".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Rc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_arc_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Arc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_ref_arc_value".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Arc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_mut_ref_arc_value".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "Arc<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_box_of_rc_ref".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "Rc<Box<Self>>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_custom_receiver_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "CustomReceiver<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_custom_receiver_ref".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "CustomReceiver<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_custom_receiver_mut_ref".into(),
            by_value: false,
            by_reference: false,
            by_mut_reference: true,
            kind: "CustomReceiver<Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_custom_receiver_with_ref_self".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "CustomReceiver<&Self>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_pinned_box".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Pin<Box<Self>>".into(),
        },
        Output {
            struct_name: "Example".into(),
            method_name: "by_pinned_ref_arc".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "Pin<&Arc<Self>>".into(),
        },
        Output {
            struct_name: "GenericExample".into(),
            method_name: "by_generic_ref".into(),
            by_value: false,
            by_reference: true,
            by_mut_reference: false,
            kind: "GenericExample<'a, T>".into(),
        },
        Output {
            struct_name: "GenericExample".into(),
            method_name: "by_generic_value".into(),
            by_value: true,
            by_reference: false,
            by_mut_reference: false,
            kind: "GenericExample<'a, T>".into(),
        },
    ];
    expected_results.sort_unstable();
    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn generic_type_param_maybe_sized() {
    get_test_data!(data, generic_type_param_maybe_sized);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let top_level_query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                name @output
                name @filter(op: "!=", value: ["$method_name"])

                generic_parameter {
                    ... on GenericTypeParameter {
                        generic_name: name @output
                        maybe_sized @output
                    }
                }
            }
        }
    }
}
"#;
    let impl_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                inherent_impl {
                    generic_parameter {
                        ... on GenericTypeParameter {
                            generic_name: name @output
                            maybe_sized @output
                        }
                    }

                    method {
                        name @output
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
                inherent_impl {
                    method {
                        name @output

                        generic_parameter {
                            ... on GenericTypeParameter {
                                generic_name: name @output
                                maybe_sized @output
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
                            maybe_sized @output
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
        maybe_sized: bool,
    }

    let mut results: Vec<Output> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        top_level_query,
        top_level_variables.clone(),
    )
    .expect("failed to run top level query")
    .chain(
        trustfall::execute_query(&schema, adapter.clone(), impl_query, variables.clone())
            .expect("failed to run impl query"),
    )
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

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "GenericStruct".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "GenericStruct".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericStruct".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericEnum".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "GenericEnum".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericEnum".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericUnion".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "GenericUnion".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericUnion".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericTrait".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "GenericTrait".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "GenericTrait".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "trait_method".into(),
            generic_name: "W".into(),
            maybe_sized: true,
        },
        Output {
            name: "trait_method".into(),
            generic_name: "X".into(),
            maybe_sized: false,
        },
        Output {
            name: "trait_method".into(),
            generic_name: "Y".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn1".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_fn1".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn1".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn2".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn3".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_fn3a".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn4".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn5".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn6".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn7".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn7a".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_fn8".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_fn8".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn8".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "generic_fn9".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "impl_trait".into(),
            generic_name: "impl GenericTrait<T, U, V>".into(),
            maybe_sized: false,
        },
        Output {
            name: "impl_trait2".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "impl_trait2".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "impl_trait2".into(),
            generic_name: "V".into(),
            maybe_sized: false,
        },
        Output {
            name: "impl_trait2".into(),
            generic_name: "impl GenericTrait<T, U, V> + ?core::marker::Sized".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_method".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "generic_method".into(),
            generic_name: "U".into(),
            maybe_sized: false,
        },
        Output {
            name: "ImplNarrowing".into(),
            generic_name: "T".into(),
            maybe_sized: true,
        },
        Output {
            name: "taking_sized_t".into(),
            generic_name: "T".into(),
            maybe_sized: false,
        },
        Output {
            name: "ImplicitlySized".into(),
            generic_name: "T".into(),
            // See the note in the test crate.
            // Our `maybe_sized` analysis is local, which believes this to be `true`.
            // A smarter, global analysis would actually determine the correct answer is `false`.
            maybe_sized: true,
        },
        Output {
            name: "ImplicitlySizedFromBuiltInTrait".into(),
            generic_name: "T".into(),
            // See the note in the test crate.
            // Our `maybe_sized` analysis is local, which believes this to be `true`.
            // A smarter, global analysis would actually determine the correct answer is `false`.
            maybe_sized: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn rustdoc_ffi_exported_functions() {
    get_test_data!(data, ffi_exported_functions);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        ffi_exported_function {
            name @output
            export_name @output

            abi {
                abi: raw_name @output
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
        export_name: String,
        abi: String,
    }

    let mut expected_results = vec![
        Output {
            name: "top_level_no_mangle_fn".into(),
            export_name: "top_level_no_mangle_fn".into(),
            abi: "C".into(),
        },
        Output {
            name: "top_level_export_name_fn".into(),
            export_name: "exported".into(),
            abi: "C-unwind".into(),
        },
        Output {
            name: "associated_fn".into(),
            export_name: "associated_fn".into(),
            abi: "C".into(),
        },
        Output {
            name: "assoc_exported_fn".into(),
            export_name: "assoc_exported".into(),
            abi: "C-unwind".into(),
        },
        Output {
            name: "method".into(),
            export_name: "method".into(),
            abi: "C".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
}

#[test]
fn rustdoc_method_export_name() {
    get_test_data!(data, ffi_exported_functions);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                owner: name @output

                inherent_impl {
                    method {
                        name @output
                        export_name @filter(op: "is_not_null") @output

                        abi {
                            abi: raw_name @output
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
        owner: String,
        name: String,
        export_name: String,
        abi: String,
    }

    let mut expected_results = vec![
        Output {
            owner: "Example".into(),
            name: "associated_fn".into(),
            export_name: "associated_fn".into(),
            abi: "C".into(),
        },
        Output {
            owner: "Example".into(),
            name: "assoc_exported_fn".into(),
            export_name: "assoc_exported".into(),
            abi: "C-unwind".into(),
        },
        Output {
            owner: "Example".into(),
            name: "method".into(),
            export_name: "method".into(),
            abi: "C".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
}

#[test]
fn target_feature() {
    get_test_data!(data, target_feature);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let top_level_query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                requires_feature {
                    feature: name @output
                    explicit @output
                    globally_enabled @output
                }
            }
        }
    }
}
"#;
    let impl_owner_inherent_methods = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                inherent_impl {
                    method {
                        name @output

                        requires_feature {
                            feature: name @output
                            explicit @output
                            globally_enabled @output
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

                    requires_feature {
                        feature: name @output
                        explicit @output
                        globally_enabled @output
                    }
                }
            }
        }
    }
}
"#;

    let variables: BTreeMap<&str, i64> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        feature: String,
        explicit: bool,
        globally_enabled: bool,
    }

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), top_level_query, variables.clone())
            .expect("failed to run top level query")
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    impl_owner_inherent_methods,
                    variables.clone(),
                )
                .expect("failed to run impl owner inherent methods query"),
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

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "top_level_fn".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "avx".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "sse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "sse4.1".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "sse4.2".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "top_level_fn".into(),
            feature: "ssse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "avx".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "avx2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "sse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "sse4.1".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "sse4.2".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_top_level_fn".into(),
            feature: "ssse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "implies_avx".into(),
            feature: "avx2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "avx".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "implies_avx".into(),
            feature: "sse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "sse4.1".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "sse4.2".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "implies_avx".into(),
            feature: "ssse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "struct_method".into(),
            feature: "avx".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "avx2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "struct_method".into(),
            feature: "sse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "sse4.1".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "sse4.2".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "struct_method".into(),
            feature: "ssse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "unsafe_struct_method".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "unsafe_struct_method".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "multiple_attrs".into(),
            feature: "bmi1".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "multiple_attrs".into(),
            feature: "bmi2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "multiple_enable_clauses".into(),
            feature: "bmi1".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "multiple_enable_clauses".into(),
            feature: "bmi2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "globally_enabled_features_are_still_listed".into(),
            feature: "sse".into(),
            explicit: true,
            globally_enabled: true,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);

    let impl_owner_impld_methods = r#"
    {
        Crate {
            item {
                ... on ImplOwner {
                    impl {
                        implemented_trait {
                            bare_name @filter(op: "=", value: ["$trait_name"])
                        }

                        method {
                            name @output

                            requires_feature {
                                feature: name @output
                                explicit @output
                                globally_enabled @output
                            }
                        }
                    }
                }
            }
        }
    }
    "#;

    let mut trait_impl_variables: BTreeMap<&str, &str> = BTreeMap::default();
    trait_impl_variables.insert("trait_name", "Trait");

    let mut results: Vec<Output> = trustfall::execute_query(
        &schema,
        adapter.clone(),
        impl_owner_impld_methods,
        trait_impl_variables,
    )
    .expect("failed to run impl owner impld methods query")
    .map(|row| row.try_into_struct().expect("shape mismatch"))
    .collect();

    results.sort_unstable();

    // We write the results in the order the items appear in the test file,
    // and sort them afterward in order to compare with the (sorted) query results.
    // This makes it easier to verify that the expected data here is correct
    // by reading it side-by-side with the file.
    let mut expected_results = vec![
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse2".into(),
            explicit: true,
            globally_enabled: true,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "avx".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "avx2".into(),
            explicit: true,
            globally_enabled: false,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse".into(),
            explicit: false,
            globally_enabled: true,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse3".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse4.1".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "sse4.2".into(),
            explicit: false,
            globally_enabled: false,
        },
        Output {
            name: "defaulted_trait_method".into(),
            feature: "ssse3".into(),
            explicit: false,
            globally_enabled: false,
        },
    ];
    expected_results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn feature_not_on_our_target_triple() {
    get_test_data!(data, feature_not_on_our_target_triple);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                requires_feature {
                    feature: name @output
                    explicit @filter(op: "=", value: ["$true"])
                    globally_enabled @output
                    valid_for_current_target @output
                }
            }
        }
    }
}
"#;

    let mut variables: BTreeMap<&str, bool> = BTreeMap::default();
    variables.insert("true", true);

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        feature: String,
        globally_enabled: bool,
        valid_for_current_target: bool,
    }

    let mut expected_results = vec![
        Output {
            name: "safe_fn".into(),
            feature: "leoncasa".into(),
            globally_enabled: false,
            valid_for_current_target: false,
        },
        Output {
            name: "unsafe_fn".into(),
            feature: "leoncasa".into(),
            globally_enabled: false,
            valid_for_current_target: false,
        },
        Output {
            name: "impossible_to_satisfy".into(),
            feature: "leoncasa".into(),
            globally_enabled: false,
            valid_for_current_target: false,
        },
        Output {
            name: "impossible_to_satisfy".into(),
            feature: "avx2".into(),
            globally_enabled: false,
            valid_for_current_target: cfg!(target_arch = "x86_64"),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<_> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables.clone())
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results,);
}

#[test]
fn function_parameters() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let functions_query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                parameter @fold {
                    params: name @output
                }
            }
        }
    }
}
"#;
    let methods_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                inherent_impl {
                    method {
                        name @output

                        parameter @fold {
                            params: name @output
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

                    parameter @fold {
                        params: name @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        params: Vec<String>,
    }

    let mut expected_results = vec![
        Output {
            name: "add".into(),
            params: vec!["left".into(), "right".into()],
        },
        Output {
            name: "fn_returns_nothing".into(),
            params: vec![],
        },
        Output {
            name: "concrete_types".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "generic_identity".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "lifetime_ref".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "two_lifetimes".into(),
            params: vec!["first".into(), "second".into()],
        },
        Output {
            name: "const_array".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "path_types".into(),
            params: vec!["value".into(), "values".into()],
        },
        Output {
            name: "lifetime_const_path_args".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "composite_types".into(),
            params: vec!["tuple".into(), "raw".into()],
        },
        Output {
            name: "function_pointer".into(),
            params: vec!["callback".into()],
        },
        Output {
            name: "function_pointer_nested_generics".into(),
            params: vec!["callback".into()],
        },
        Output {
            name: "dyn_trait_lifetime".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "dyn_fn_two_arg".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "dyn_fn_pointer_output".into(),
            params: vec!["callback".into(), "callback_with_lifetime".into()],
        },
        Output {
            name: "impl_trait_param".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            params: vec!["generic".into(), "first".into(), "second".into()],
        },
        Output {
            name: "nested_impl_trait_params".into(),
            params: vec![
                "generic".into(),
                "borrowed".into(),
                "values".into(),
                "nested_tuple".into(),
            ],
        },
        Output {
            name: "nested_assoc_impl_trait_param".into(),
            params: vec!["value".into(), "other".into()],
        },
        Output {
            name: "impl_trait_numbering_baseline".into(),
            params: vec!["first".into(), "second".into()],
        },
        Output {
            name: "impl_trait_numbering_nested_first".into(),
            params: vec!["first".into(), "second".into()],
        },
        Output {
            name: "impl_trait_numbering_nested_second".into(),
            params: vec!["first".into(), "second".into()],
        },
        Output {
            name: "multiple_impl_traits_single_param".into(),
            params: vec!["pair".into(), "later".into()],
        },
        Output {
            name: "maybe_sized_impl_trait".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "impl_fn_bound".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "impl_fn_lifetime_bound".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "impl_fn_two_args_bound".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "impl_trait_lifetime_return".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            params: vec![
                "c_unwind".into(),
                "system".into(),
                "system_unwind".into(),
                "win64".into(),
                "sysv64".into(),
            ],
        },
        Output {
            name: "dyn_pointer_and_mut_ref".into(),
            params: vec!["pointer".into(), "borrowed".into()],
        },
        Output {
            name: "borrowed_opaque_return".into(),
            params: vec![],
        },
        Output {
            name: "raw_pointer_opaque_return".into(),
            params: vec![],
        },
        Output {
            name: "fn_bound_pointer_to_dyn_return".into(),
            params: vec![],
        },
        Output {
            name: "sorted_higher_ranked_return_bounds".into(),
            params: vec![],
        },
        Output {
            name: "repeated_higher_ranked_lifetime_name_return".into(),
            params: vec![],
        },
        Output {
            name: "raw_pointer_impl_trait".into(),
            params: vec!["value".into(), "mutable".into()],
        },
        Output {
            name: "slice_and_array_impl_trait".into(),
            params: vec!["value".into(), "array".into()],
        },
        Output {
            name: "unit_and_single_tuple".into(),
            params: vec!["unit".into(), "single".into()],
        },
        Output {
            name: "higher_ranked_fn_pointer".into(),
            params: vec!["callback".into()],
        },
        Output {
            name: "nested_higher_ranked_fn_pointer".into(),
            params: vec!["callback".into()],
        },
        Output {
            name: "unsafe_c_variadic_pointer".into(),
            params: vec!["callback".into()],
        },
        Output {
            name: "impl_trait_return".into(),
            params: vec![],
        },
        Output {
            name: "precise_capture_return".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "precise_capture_lifetime".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "precise_capture_const".into(),
            params: vec!["value".into()],
        },
        Output {
            name: "qualified_path_assoc_arg".into(),
            params: vec!["pair".into()],
        },
        Output {
            name: "generic_assoc_arg".into(),
            params: vec!["pair".into()],
        },
        Output {
            name: "parent_and_method".into(),
            params: vec![
                "self".into(),
                "owner".into(),
                "extra".into(),
                "method".into(),
            ],
        },
        Output {
            name: "parent_lifetime_const".into(),
            params: vec!["self".into(), "owner".into(), "method".into()],
        },
        Output {
            name: "add_method".into(),
            params: vec!["self".into(), "left".into(), "right".into()],
        },
        Output {
            name: "method_returns_nothing".into(),
            params: vec!["self".into()],
        },
        Output {
            name: "combine".into(),
            params: vec!["self".into(), "owner".into(), "method".into()],
        },
        Output {
            name: "pin_box_self".into(),
            params: vec!["self".into()],
        },
        Output {
            name: "add_trait_fn".into(),
            params: vec!["self".into(), "value".into()],
        },
        Output {
            name: "trait_fn_returns_nothing".into(),
            params: vec!["self".into()],
        },
        Output {
            name: "fn_output_dyn".into(),
            params: vec!["self".into()],
        },
        Output {
            name: "fn_output_dyn_single_bound".into(),
            params: vec!["self".into()],
        },
        Output {
            name: "combine_trait".into(),
            params: vec!["self".into(), "owner".into(), "method".into()],
        },
        Output {
            name: "self_qualified".into(),
            params: vec!["pair".into()],
        },
        Output {
            name: "default_combine".into(),
            params: vec!["self".into(), "owner".into(), "method".into()],
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), functions_query, variables.clone())
            .expect("failed to run top level query")
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    methods_query,
                    variables.clone(),
                )
                .expect("failed to run impl owner inherent methods query"),
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

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_return_value() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let functions_query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                return_value {
                    is_unit @output
                }
            }
        }
    }
}
"#;
    let methods_query = r#"
{
    Crate {
        item {
            ... on ImplOwner {
                inherent_impl {
                    method {
                        name @output

                        return_value {
                            is_unit @output
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

                    return_value {
                        is_unit @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        is_unit: bool,
    }

    let mut expected_results = vec![
        Output {
            name: "add".into(),
            is_unit: false,
        },
        Output {
            name: "fn_returns_nothing".into(),
            is_unit: true,
        },
        Output {
            name: "concrete_types".into(),
            is_unit: false,
        },
        Output {
            name: "generic_identity".into(),
            is_unit: false,
        },
        Output {
            name: "lifetime_ref".into(),
            is_unit: false,
        },
        Output {
            name: "two_lifetimes".into(),
            is_unit: false,
        },
        Output {
            name: "const_array".into(),
            is_unit: false,
        },
        Output {
            name: "path_types".into(),
            is_unit: false,
        },
        Output {
            name: "lifetime_const_path_args".into(),
            is_unit: true,
        },
        Output {
            name: "composite_types".into(),
            is_unit: false,
        },
        Output {
            name: "function_pointer".into(),
            is_unit: false,
        },
        Output {
            name: "function_pointer_nested_generics".into(),
            is_unit: false,
        },
        Output {
            name: "dyn_trait_lifetime".into(),
            is_unit: false,
        },
        Output {
            name: "dyn_fn_two_arg".into(),
            is_unit: true,
        },
        Output {
            name: "dyn_fn_pointer_output".into(),
            is_unit: true,
        },
        Output {
            name: "impl_trait_param".into(),
            is_unit: false,
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            is_unit: false,
        },
        Output {
            name: "nested_impl_trait_params".into(),
            is_unit: false,
        },
        Output {
            name: "nested_assoc_impl_trait_param".into(),
            is_unit: false,
        },
        Output {
            name: "impl_trait_numbering_baseline".into(),
            is_unit: true,
        },
        Output {
            name: "impl_trait_numbering_nested_first".into(),
            is_unit: false,
        },
        Output {
            name: "impl_trait_numbering_nested_second".into(),
            is_unit: false,
        },
        Output {
            name: "multiple_impl_traits_single_param".into(),
            is_unit: true,
        },
        Output {
            name: "maybe_sized_impl_trait".into(),
            is_unit: true,
        },
        Output {
            name: "impl_fn_bound".into(),
            is_unit: true,
        },
        Output {
            name: "impl_fn_lifetime_bound".into(),
            is_unit: true,
        },
        Output {
            name: "impl_fn_two_args_bound".into(),
            is_unit: true,
        },
        Output {
            name: "impl_trait_lifetime_return".into(),
            is_unit: false,
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            is_unit: true,
        },
        Output {
            name: "dyn_pointer_and_mut_ref".into(),
            is_unit: true,
        },
        Output {
            name: "borrowed_opaque_return".into(),
            is_unit: false,
        },
        Output {
            name: "raw_pointer_opaque_return".into(),
            is_unit: false,
        },
        Output {
            name: "fn_bound_pointer_to_dyn_return".into(),
            is_unit: false,
        },
        Output {
            name: "sorted_higher_ranked_return_bounds".into(),
            is_unit: false,
        },
        Output {
            name: "repeated_higher_ranked_lifetime_name_return".into(),
            is_unit: false,
        },
        Output {
            name: "raw_pointer_impl_trait".into(),
            is_unit: true,
        },
        Output {
            name: "slice_and_array_impl_trait".into(),
            is_unit: true,
        },
        Output {
            name: "unit_and_single_tuple".into(),
            is_unit: true,
        },
        Output {
            name: "higher_ranked_fn_pointer".into(),
            is_unit: true,
        },
        Output {
            name: "nested_higher_ranked_fn_pointer".into(),
            is_unit: true,
        },
        Output {
            name: "unsafe_c_variadic_pointer".into(),
            is_unit: true,
        },
        Output {
            name: "impl_trait_return".into(),
            is_unit: false,
        },
        Output {
            name: "precise_capture_return".into(),
            is_unit: false,
        },
        Output {
            name: "precise_capture_lifetime".into(),
            is_unit: false,
        },
        Output {
            name: "precise_capture_const".into(),
            is_unit: false,
        },
        Output {
            name: "qualified_path_assoc_arg".into(),
            is_unit: true,
        },
        Output {
            name: "generic_assoc_arg".into(),
            is_unit: true,
        },
        Output {
            name: "parent_and_method".into(),
            is_unit: false,
        },
        Output {
            name: "parent_lifetime_const".into(),
            is_unit: false,
        },
        Output {
            name: "add_method".into(),
            is_unit: false,
        },
        Output {
            name: "method_returns_nothing".into(),
            is_unit: true,
        },
        Output {
            name: "combine".into(),
            is_unit: false,
        },
        Output {
            name: "pin_box_self".into(),
            is_unit: true,
        },
        Output {
            name: "add_trait_fn".into(),
            is_unit: false,
        },
        Output {
            name: "trait_fn_returns_nothing".into(),
            is_unit: true,
        },
        Output {
            name: "fn_output_dyn".into(),
            is_unit: false,
        },
        Output {
            name: "fn_output_dyn_single_bound".into(),
            is_unit: false,
        },
        Output {
            name: "combine_trait".into(),
            is_unit: false,
        },
        Output {
            name: "self_qualified".into(),
            is_unit: true,
        },
        Output {
            name: "default_combine".into(),
            is_unit: false,
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), functions_query, variables.clone())
            .expect("failed to run top level query")
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    methods_query,
                    variables.clone(),
                )
                .expect("failed to run impl owner inherent methods query"),
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

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_parameter_normalized_type_signatures() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                parameter {
                    position @output
                    param_name: name @output
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        position: u64,
        param_name: String,
        signature: String,
    }

    let mut expected_results = vec![
        Output {
            name: "add".into(),
            position: 1,
            param_name: "left".into(),
            signature: "u64".into(),
        },
        Output {
            name: "add".into(),
            position: 2,
            param_name: "right".into(),
            signature: "u64".into(),
        },
        Output {
            name: "concrete_types".into(),
            position: 1,
            param_name: "value".into(),
            signature: "u64".into(),
        },
        Output {
            name: "generic_identity".into(),
            position: 1,
            param_name: "value".into(),
            signature: "T1".into(),
        },
        Output {
            name: "lifetime_ref".into(),
            position: 1,
            param_name: "value".into(),
            signature: "&'a str".into(),
        },
        Output {
            name: "two_lifetimes".into(),
            position: 1,
            param_name: "first".into(),
            signature: "&'a u8".into(),
        },
        Output {
            name: "two_lifetimes".into(),
            position: 2,
            param_name: "second".into(),
            signature: "&'b u8".into(),
        },
        Output {
            name: "const_array".into(),
            position: 1,
            param_name: "value".into(),
            signature: "[u8; C1]".into(),
        },
        Output {
            name: "path_types".into(),
            position: 1,
            param_name: "value".into(),
            signature: "::function_params_and_return_value::PublicType<u8>".into(),
        },
        Output {
            name: "path_types".into(),
            position: 2,
            param_name: "values".into(),
            signature: "::alloc::vec::Vec<::function_params_and_return_value::PublicType<u8>>"
                .into(),
        },
        Output {
            name: "lifetime_const_path_args".into(),
            position: 1,
            param_name: "value".into(),
            signature: "::function_params_and_return_value::LifetimeConst<'a, C1>".into(),
        },
        Output {
            name: "composite_types".into(),
            position: 1,
            param_name: "tuple".into(),
            signature: "(&'a [T1], *const T1, fn(T1) -> T1, [u8; C1])".into(),
        },
        Output {
            name: "composite_types".into(),
            position: 2,
            param_name: "raw".into(),
            signature: "*mut T1".into(),
        },
        Output {
            name: "function_pointer".into(),
            position: 1,
            param_name: "callback".into(),
            signature: "for<'b1> unsafe fn(&'b1 u8) -> &'b1 u8".into(),
        },
        Output {
            name: "function_pointer_nested_generics".into(),
            position: 1,
            param_name: "callback".into(),
            signature: "for<'b1> fn(&'a T1, &'b1 [T1; C1]) -> &'b1 T1".into(),
        },
        Output {
            name: "dyn_trait_lifetime".into(),
            position: 1,
            param_name: "value".into(),
            signature: "::alloc::boxed::Box<dyn ::core::marker::Send + ::core::marker::Sync + 'a>"
                .into(),
        },
        Output {
            name: "dyn_fn_two_arg".into(),
            position: 1,
            param_name: "value".into(),
            signature: "::alloc::boxed::Box<dyn ::core::ops::function::Fn(u8, u16) -> u32>".into(),
        },
        Output {
            name: "dyn_fn_pointer_output".into(),
            position: 1,
            param_name: "callback".into(),
            signature:
                "::alloc::boxed::Box<dyn ::core::ops::function::Fn() -> *const dyn ::core::marker::Send>"
                    .into(),
        },
        Output {
            name: "dyn_fn_pointer_output".into(),
            position: 2,
            param_name: "callback_with_lifetime".into(),
            signature:
                "::alloc::boxed::Box<dyn ::core::ops::function::Fn() -> *const (dyn ::core::marker::Send) + 'static>"
                    .into(),
        },
        Output {
            name: "impl_trait_param".into(),
            position: 1,
            param_name: "value".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            position: 1,
            param_name: "generic".into(),
            signature: "T1".into(),
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            position: 2,
            param_name: "first".into(),
            signature: "IT2_1".into(),
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            position: 3,
            param_name: "second".into(),
            signature: "IT3_1".into(),
        },
        Output {
            name: "nested_impl_trait_params".into(),
            position: 1,
            param_name: "generic".into(),
            signature: "T1".into(),
        },
        Output {
            name: "nested_impl_trait_params".into(),
            position: 2,
            param_name: "borrowed".into(),
            signature: "&IT2_1".into(),
        },
        Output {
            name: "nested_impl_trait_params".into(),
            position: 3,
            param_name: "values".into(),
            signature: "::alloc::vec::Vec<IT3_1>".into(),
        },
        Output {
            name: "nested_impl_trait_params".into(),
            position: 4,
            param_name: "nested_tuple".into(),
            signature: "(IT4_1, T1)".into(),
        },
        Output {
            name: "nested_assoc_impl_trait_param".into(),
            position: 1,
            param_name: "value".into(),
            signature: "IT1_2".into(),
        },
        Output {
            name: "nested_assoc_impl_trait_param".into(),
            position: 2,
            param_name: "other".into(),
            signature: "IT2_1".into(),
        },
        Output {
            name: "impl_trait_numbering_baseline".into(),
            position: 1,
            param_name: "first".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "impl_trait_numbering_baseline".into(),
            position: 2,
            param_name: "second".into(),
            signature: "IT2_1".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_first".into(),
            position: 1,
            param_name: "first".into(),
            signature: "IT1_2".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_first".into(),
            position: 2,
            param_name: "second".into(),
            signature: "IT2_1".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_second".into(),
            position: 1,
            param_name: "first".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_second".into(),
            position: 2,
            param_name: "second".into(),
            signature: "IT2_2".into(),
        },
        Output {
            name: "multiple_impl_traits_single_param".into(),
            position: 1,
            param_name: "pair".into(),
            signature: "(IT1_1, IT1_2)".into(),
        },
        Output {
            name: "multiple_impl_traits_single_param".into(),
            position: 2,
            param_name: "later".into(),
            signature: "IT2_1".into(),
        },
        Output {
            name: "maybe_sized_impl_trait".into(),
            position: 1,
            param_name: "value".into(),
            signature: "&IT1_1".into(),
        },
        Output {
            name: "impl_fn_bound".into(),
            position: 1,
            param_name: "value".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "impl_fn_lifetime_bound".into(),
            position: 1,
            param_name: "value".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "impl_fn_two_args_bound".into(),
            position: 1,
            param_name: "value".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "impl_trait_lifetime_return".into(),
            position: 1,
            param_name: "value".into(),
            signature: "&'a u8".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            position: 1,
            param_name: "c_unwind".into(),
            signature: "extern \"C-unwind\" fn(u8) -> u8".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            position: 2,
            param_name: "system".into(),
            signature: "extern \"system\" fn(u8) -> u8".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            position: 3,
            param_name: "system_unwind".into(),
            signature: "extern \"system-unwind\" fn(u8) -> u8".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            position: 4,
            param_name: "win64".into(),
            signature: "extern \"win64\" fn(u8) -> u8".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            position: 5,
            param_name: "sysv64".into(),
            signature: "extern \"sysv64\" fn(u8) -> u8".into(),
        },
        Output {
            name: "dyn_pointer_and_mut_ref".into(),
            position: 1,
            param_name: "pointer".into(),
            signature: "*const (dyn ::core::marker::Send + ::core::marker::Sync + 'a)".into(),
        },
        Output {
            name: "dyn_pointer_and_mut_ref".into(),
            position: 2,
            param_name: "borrowed".into(),
            signature: "&'a mut (dyn ::core::marker::Send + ::core::marker::Sync)".into(),
        },
        Output {
            name: "raw_pointer_impl_trait".into(),
            position: 1,
            param_name: "value".into(),
            signature: "*const IT1_1".into(),
        },
        Output {
            name: "raw_pointer_impl_trait".into(),
            position: 2,
            param_name: "mutable".into(),
            signature: "*mut IT2_1".into(),
        },
        Output {
            name: "slice_and_array_impl_trait".into(),
            position: 1,
            param_name: "value".into(),
            signature: "&[IT1_1]".into(),
        },
        Output {
            name: "slice_and_array_impl_trait".into(),
            position: 2,
            param_name: "array".into(),
            signature: "[IT2_1; 3]".into(),
        },
        Output {
            name: "unit_and_single_tuple".into(),
            position: 1,
            param_name: "unit".into(),
            signature: "()".into(),
        },
        Output {
            name: "unit_and_single_tuple".into(),
            position: 2,
            param_name: "single".into(),
            signature: "(IT2_1,)".into(),
        },
        Output {
            name: "higher_ranked_fn_pointer".into(),
            position: 1,
            param_name: "callback".into(),
            signature: "for<'b1> fn(&'b1 u8) -> &'b1 u8".into(),
        },
        Output {
            name: "nested_higher_ranked_fn_pointer".into(),
            position: 1,
            param_name: "callback".into(),
            signature: "for<'b1> fn(&'b1 (), for<'b2> fn(&'b2 ()))".into(),
        },
        Output {
            name: "unsafe_c_variadic_pointer".into(),
            position: 1,
            param_name: "callback".into(),
            signature: "unsafe extern \"C\" fn(u8, ...) -> u8".into(),
        },
        Output {
            name: "precise_capture_return".into(),
            position: 1,
            param_name: "value".into(),
            signature: "T1".into(),
        },
        Output {
            name: "precise_capture_lifetime".into(),
            position: 1,
            param_name: "value".into(),
            signature: "&'a T1".into(),
        },
        Output {
            name: "precise_capture_const".into(),
            position: 1,
            param_name: "value".into(),
            signature: "[u8; C1]".into(),
        },
        Output {
            name: "qualified_path_assoc_arg".into(),
            position: 1,
            param_name: "pair".into(),
            signature:
                "(<T1 as ::function_params_and_return_value::Provider>::Assoc<IT1_1>, IT1_2)".into(),
        },
        Output {
            name: "generic_assoc_arg".into(),
            position: 1,
            param_name: "pair".into(),
            signature: "(T1::Assoc<IT1_1>, IT1_2)".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Query every function in `assoc_constraint_order` so new fixture examples
/// require an expected output here by default. Associated-item constraints are
/// sorted before parameter-position `impl Trait` names are assigned, while
/// `impl Trait` occurrences in generic bounds are consumed but do not appear in
/// the normalized parameter type signature.
#[test]
fn function_parameter_normalized_type_signature_handles_assoc_constraint_impl_trait() {
    get_test_data!(data, assoc_constraint_order);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                parameter {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let expected_results = vec![
        Output {
            name: "a_then_b".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = IT1_1, B = IT1_2>>".into(),
        },
        Output {
            name: "b_then_a".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = IT1_1, B = IT1_2>>".into(),
        },
        Output {
            name: "bound_a_then_b".into(),
            signature: "T1".into(),
        },
        Output {
            name: "bound_b_then_a".into(),
            signature: "T1".into(),
        },
        Output {
            name: "same_synthetic_names".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = IT1_1, B = IT1_2>>".into(),
        },
        Output {
            name: "gat_bound_u8_then_u16".into(),
            signature: "T1".into(),
        },
        Output {
            name: "repeated_gat_bound_u8_then_u16".into(),
            signature: "T1".into(),
        },
        Output {
            name: "generic_bound_before_input_impl_trait".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "generic_bound_before_input_impl_trait".into(),
            signature: "T1".into(),
        },
        Output {
            name: "top_level_assoc_bound_counts_hidden_impls".into(),
            signature: "(IT1_2, IT1_3)".into(),
        },
        Output {
            name: "assoc_constraint_args_count_before_outer_impl".into(),
            signature: "(IT1_3, IT1_4)".into(),
        },
        Output {
            name: "hrtb_bound_counts_before_later_impl".into(),
            signature: "(IT1_1, IT1_2)".into(),
        },
        Output {
            name: "hrtb_two_lifetime_bound_counts_before_later_impl".into(),
            signature: "(IT1_1, IT1_2)".into(),
        },
        Output {
            name: "lifetime_const_generic_arg_counts_before_later_impl".into(),
            signature: "(IT1_1, IT1_2)".into(),
        },
        Output {
            name: "complex_constraint_a_then_b".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = ::alloc::boxed::Box<dyn for<'b1> ::core::ops::function::Fn(&'b1 u8) -> &'b1 u8 + 'a>, B = IT1_1>>".into(),
        },
        Output {
            name: "complex_constraint_b_then_a".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = ::alloc::boxed::Box<dyn for<'b1> ::core::ops::function::Fn(&'b1 u8) -> &'b1 u8 + 'a>, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_type_shapes".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = (&'a [u8], [u8; C1]), B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_function_pointer".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = fn(u8) -> u8, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_higher_ranked_function_pointer".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = for<'b1> fn(&'b1 u8) -> &'b1 u8, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_two_lifetime_function_pointer".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = for<'b1, 'b2> fn(&'b1 u8, &'b2 u8) -> &'b2 u8, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_higher_ranked_two_constraints_b_then_a".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = for<'b1> fn(&'b1 u8) -> &'b1 u8, B = for<'b2> fn(&'b2 u16) -> &'b2 u16>>".into(),
        },
        Output {
            name: "constraint_dyn_fn_trait_two_args".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = ::alloc::boxed::Box<dyn ::core::ops::function::Fn(u8, u16) -> u32>, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_qualified_path".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = <T1 as ::assoc_constraint_order::Provider>::Assoc<IT1_1>, B = IT1_2>>".into(),
        },
        Output {
            name: "where_bound_after_input_impl_trait".into(),
            signature: "IT1_1".into(),
        },
        Output {
            name: "where_bound_after_input_impl_trait".into(),
            signature: "T1".into(),
        },
        Output {
            name: "constraint_multiple_bounds".into(),
            signature: "IT1_2".into(),
        },
        Output {
            name: "constraint_mutable_reference".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = &'static mut IT1_1, B = IT1_2>>".into(),
        },
        Output {
            name: "constraint_raw_pointers".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = *const IT1_1, B = *mut IT1_2>>".into(),
        },
        Output {
            name: "constraint_slice_and_array".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = &'static [IT1_1], B = [IT1_2; 3]>>".into(),
        },
        Output {
            name: "constraint_single_tuple".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = (IT1_1,), B = IT1_2>>".into(),
        },
        Output {
            name: "constraint_dyn_multi_trait".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = ::alloc::boxed::Box<dyn ::core::marker::Send + ::core::marker::Sync + 'a>, B = IT1_1>>".into(),
        },
        Output {
            name: "constraint_dyn_multi_trait_reverse".into(),
            signature: "::alloc::boxed::Box<dyn ::assoc_constraint_order::AssocConstraintOrder<A = ::alloc::boxed::Box<dyn ::core::marker::Send + ::core::marker::Sync + 'a>, B = IT1_1>>".into(),
        },
    ];
    let mut sorted_expected_results = expected_results.clone();
    sorted_expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(sorted_expected_results, results);
}

#[test]
fn function_return_normalized_type_signature_handles_assoc_constraint_bound() {
    get_test_data!(data, assoc_constraint_order);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                return_value {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, FieldValue> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let unit_return_functions = [
        "a_then_b",
        "b_then_a",
        "bound_a_then_b",
        "bound_b_then_a",
        "same_synthetic_names",
        "gat_bound_u8_then_u16",
        "repeated_gat_bound_u8_then_u16",
        "generic_bound_before_input_impl_trait",
        "top_level_assoc_bound_counts_hidden_impls",
        "assoc_constraint_args_count_before_outer_impl",
        "hrtb_bound_counts_before_later_impl",
        "hrtb_two_lifetime_bound_counts_before_later_impl",
        "lifetime_const_generic_arg_counts_before_later_impl",
        "complex_constraint_a_then_b",
        "complex_constraint_b_then_a",
        "constraint_type_shapes",
        "constraint_function_pointer",
        "constraint_higher_ranked_function_pointer",
        "constraint_two_lifetime_function_pointer",
        "constraint_higher_ranked_two_constraints_b_then_a",
        "constraint_dyn_fn_trait_two_args",
        "constraint_qualified_path",
        "where_bound_after_input_impl_trait",
        "constraint_multiple_bounds",
        "constraint_raw_pointers",
        "constraint_slice_and_array",
        "constraint_mutable_reference",
        "constraint_single_tuple",
        "constraint_dyn_multi_trait",
        "constraint_dyn_multi_trait_reverse",
    ];

    let mut expected_results = unit_return_functions
        .into_iter()
        .map(|name| Output {
            name: name.into(),
            signature: "()".into(),
        })
        .chain([
            Output {
                name: "return_assoc_constraint_bound".into(),
                signature:
                    "impl ::assoc_constraint_order::AssocConstraintOrder<A: ::core::clone::Clone + ::core::marker::Copy, B = u8>"
                        .into(),
            },
            Output {
                name: "return_generic_assoc_constraint_bound".into(),
                signature:
                    "impl ::assoc_constraint_order::GenericAssoc<u8, A: ::core::clone::Clone + ::core::marker::Copy>"
                        .into(),
            },
            Output {
                name: "return_gat_assoc_constraint_bound".into(),
                signature:
                    "impl ::assoc_constraint_order::HasGenericItem<Item<u8>: ::core::clone::Clone + ::core::marker::Copy>"
                        .into(),
            },
            Output {
                name: "return_nested_dyn_bound".into(),
                signature: "impl ::assoc_constraint_order::HasItem<Item = dyn ::assoc_constraint_order::RealTrait + ::core::marker::Send>".into(),
            },
            Output {
                name: "return_nested_parenthesized_dyn_bound".into(),
                signature: "impl ::assoc_constraint_order::HasItem<Item = fn() -> *const (dyn ::assoc_constraint_order::RealTrait + ::core::marker::Send)>".into(),
            },
            Output {
                name: "return_nested_opaque_bound_clone_then_copy".into(),
                signature: "impl ::assoc_constraint_order::HasItem<Item: ::assoc_constraint_order::Takes<impl ::core::clone::Clone, Witness = ()> + ::assoc_constraint_order::Takes<impl ::core::marker::Copy, Witness = ()>>".into(),
            },
            Output {
                name: "return_nested_opaque_bound_copy_then_clone".into(),
                signature: "impl ::assoc_constraint_order::HasItem<Item: ::assoc_constraint_order::Takes<impl ::core::clone::Clone, Witness = ()> + ::assoc_constraint_order::Takes<impl ::core::marker::Copy, Witness = ()>>".into(),
            },
            Output {
                name: "return_function_pointer_bound_safe_then_unsafe".into(),
                signature: "impl ::assoc_constraint_order::Takes<fn(u8) -> u8> + ::assoc_constraint_order::Takes<unsafe extern \"C\" fn(u8, ...) -> u8>".into(),
            },
            Output {
                name: "return_function_pointer_bound_unsafe_then_safe".into(),
                signature: "impl ::assoc_constraint_order::Takes<fn(u8) -> u8> + ::assoc_constraint_order::Takes<unsafe extern \"C\" fn(u8, ...) -> u8>".into(),
            },
            Output {
                name: "return_assoc_constraints_b_then_a".into(),
                signature: "impl ::assoc_constraint_order::AssocConstraintOrder<A = u8, B = u8>".into(),
            },
            Output {
                name: "return_const_arg_sort_key".into(),
                signature: "impl ::assoc_constraint_order::TakesConst<1> + ::assoc_constraint_order::TakesConst<2>".into(),
            },
            Output {
                name: "return_function_pointer_signature_sort_key".into(),
                signature: "impl ::assoc_constraint_order::Takes<fn(u16)> + ::assoc_constraint_order::Takes<fn(u8)>".into(),
            },
            Output {
                name: "return_function_pointer_header_sort_key".into(),
                signature: "impl ::assoc_constraint_order::Takes<fn(u8)> + ::assoc_constraint_order::Takes<unsafe fn(u8)>".into(),
            },
            Output {
                name: "return_function_pointer_abi_sort_key".into(),
                signature: "impl ::assoc_constraint_order::Takes<unsafe extern \"C\" fn(u8)> + ::assoc_constraint_order::Takes<unsafe extern \"C-unwind\" fn(u8)>".into(),
            },
        ])
        .collect::<Vec<_>>();
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_return_normalized_type_signatures() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                return_value {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut expected_results = vec![
        Output {
            name: "add".into(),
            signature: "u64".into(),
        },
        Output {
            name: "fn_returns_nothing".into(),
            signature: "()".into(),
        },
        Output {
            name: "concrete_types".into(),
            signature: "bool".into(),
        },
        Output {
            name: "generic_identity".into(),
            signature: "T1".into(),
        },
        Output {
            name: "lifetime_ref".into(),
            signature: "&'a str".into(),
        },
        Output {
            name: "two_lifetimes".into(),
            signature: "(&'a u8, &'b u8)".into(),
        },
        Output {
            name: "const_array".into(),
            signature: "[u8; C1]".into(),
        },
        Output {
            name: "path_types".into(),
            signature: "::core::option::Option<::function_params_and_return_value::PublicType<u8>>"
                .into(),
        },
        Output {
            name: "lifetime_const_path_args".into(),
            signature: "()".into(),
        },
        Output {
            name: "composite_types".into(),
            signature: "(&'a [T1], *mut T1)".into(),
        },
        Output {
            name: "function_pointer".into(),
            signature: "for<'b1> unsafe fn(&'b1 u8) -> &'b1 u8".into(),
        },
        Output {
            name: "function_pointer_nested_generics".into(),
            signature: "for<'b1> fn(&'a T1, &'b1 [T1; C1]) -> &'b1 T1".into(),
        },
        Output {
            name: "nested_higher_ranked_fn_pointer".into(),
            signature: "()".into(),
        },
        Output {
            name: "dyn_trait_lifetime".into(),
            signature: "::alloc::boxed::Box<dyn ::core::marker::Send + ::core::marker::Sync + 'a>"
                .into(),
        },
        Output {
            name: "dyn_fn_two_arg".into(),
            signature: "()".into(),
        },
        Output {
            name: "dyn_fn_pointer_output".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_trait_param".into(),
            signature: "::alloc::string::String".into(),
        },
        Output {
            name: "generic_and_impl_trait_params".into(),
            signature: "(T1, ::alloc::string::String)".into(),
        },
        Output {
            name: "nested_impl_trait_params".into(),
            signature: "T1".into(),
        },
        Output {
            name: "nested_assoc_impl_trait_param".into(),
            signature: "usize".into(),
        },
        Output {
            name: "impl_trait_numbering_baseline".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_first".into(),
            signature: "usize".into(),
        },
        Output {
            name: "impl_trait_numbering_nested_second".into(),
            signature: "usize".into(),
        },
        Output {
            name: "multiple_impl_traits_single_param".into(),
            signature: "()".into(),
        },
        Output {
            name: "maybe_sized_impl_trait".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_fn_bound".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_fn_lifetime_bound".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_fn_two_args_bound".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_trait_lifetime_return".into(),
            signature: "impl 'a + ::core::clone::Clone".into(),
        },
        Output {
            name: "abi_variant_fn_pointers".into(),
            signature: "()".into(),
        },
        Output {
            name: "dyn_pointer_and_mut_ref".into(),
            signature: "()".into(),
        },
        Output {
            name: "borrowed_opaque_return".into(),
            signature: "&'static (impl ::core::clone::Clone + ::core::marker::Copy)".into(),
        },
        Output {
            name: "raw_pointer_opaque_return".into(),
            signature: "*const (impl ::core::clone::Clone + ::core::marker::Copy)".into(),
        },
        Output {
            name: "fn_bound_pointer_to_dyn_return".into(),
            signature:
                "impl ::core::clone::Clone + ::core::ops::function::Fn() -> *const (dyn ::core::marker::Send + ::core::marker::Sync)"
                    .into(),
        },
        Output {
            name: "sorted_higher_ranked_return_bounds".into(),
            signature:
                "impl for<'b1> ::function_params_and_return_value::BinderTraitA<&'b1 ()> + for<'b2> ::function_params_and_return_value::BinderTraitB<&'b2 ()>"
                    .into(),
        },
        Output {
            name: "repeated_higher_ranked_lifetime_name_return".into(),
            signature:
                "impl for<'b1> ::function_params_and_return_value::BinderTraitA<&'b1 ()> + for<'b2> ::function_params_and_return_value::BinderTraitB<&'b2 ()>"
                    .into(),
        },
        Output {
            name: "raw_pointer_impl_trait".into(),
            signature: "()".into(),
        },
        Output {
            name: "slice_and_array_impl_trait".into(),
            signature: "()".into(),
        },
        Output {
            name: "unit_and_single_tuple".into(),
            signature: "()".into(),
        },
        Output {
            name: "higher_ranked_fn_pointer".into(),
            signature: "()".into(),
        },
        Output {
            name: "unsafe_c_variadic_pointer".into(),
            signature: "()".into(),
        },
        Output {
            name: "impl_trait_return".into(),
            signature: "impl ::core::iter::traits::iterator::Iterator<Item = u8>".into(),
        },
        Output {
            name: "precise_capture_return".into(),
            signature: "impl ::core::clone::Clone + use<T1>".into(),
        },
        Output {
            name: "precise_capture_lifetime".into(),
            signature: "impl ::core::clone::Clone + use<'a, T1>".into(),
        },
        Output {
            name: "precise_capture_const".into(),
            signature: "impl ::core::clone::Clone + use<C1>".into(),
        },
        Output {
            name: "qualified_path_assoc_arg".into(),
            signature: "()".into(),
        },
        Output {
            name: "generic_assoc_arg".into(),
            signature: "()".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

/// Query every function in the unstable `impl_trait_in_fn_trait_return` crate
/// so each fixture example has an expected normalized signature here.
#[test]
fn unstable_impl_trait_in_fn_trait_return_normalized_type_signatures() {
    get_test_data!(data, impl_trait_in_fn_trait_return);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                name @output

                return_value {
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables: BTreeMap<&str, bool> = BTreeMap::default();

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        name: String,
        signature: String,
    }

    let mut expected_results = vec![
        Output {
            name: "fn_bound_opaque_return".into(),
            signature:
                "impl ::core::clone::Clone + ::core::ops::function::Fn() -> (impl ::core::clone::Clone + ::core::marker::Copy)"
                    .into(),
        },
        Output {
            name: "fn_bound_opaque_return_first".into(),
            signature:
                "impl ::core::ops::function::Fn() -> (impl ::core::clone::Clone + ::core::marker::Copy) + ::impl_trait_in_fn_trait_return::Zed"
                    .into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), query, variables)
            .expect("failed to run query")
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();
    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn method_normalized_type_signatures_include_parent_generics() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let inherent_query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "=", value: ["$inherent_owner"]) @output

                inherent_impl {
                    method {
                        method_name: name @filter(op: "one_of", value: ["$inherent_methods"]) @output

                        parameter {
                            position @output
                            param_name: name @output
                            normalized_type_signature {
                                param_signature: signature @output
                            }
                        }

                        return_value {
                            normalized_type_signature {
                                return_signature: signature @output
                            }
                        }
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
                owner: name @filter(op: "=", value: ["$trait_owner"]) @output

                method {
                    method_name: name @filter(op: "=", value: ["$trait_method"]) @output

                    parameter {
                        position @output
                        param_name: name @output
                        normalized_type_signature {
                            param_signature: signature @output
                        }
                    }

                    return_value {
                        normalized_type_signature {
                            return_signature: signature @output
                        }
                    }
                }
            }
        }
    }
}
"#;
    let trait_impl_query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "=", value: ["$trait_impl_owner"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$trait_owner"])
                    }

                    method {
                        method_name: name @filter(op: "=", value: ["$trait_method"]) @output

                        parameter {
                            position @output
                            param_name: name @output
                            normalized_type_signature {
                                param_signature: signature @output
                            }
                        }

                        return_value {
                            normalized_type_signature {
                                return_signature: signature @output
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;
    let default_trait_impl_query = r#"
{
    Crate {
        item {
            ... on Struct {
                owner: name @filter(op: "=", value: ["$default_trait_impl_owner"]) @output

                impl {
                    implemented_trait {
                        bare_name @filter(op: "=", value: ["$default_trait_owner"])
                    }

                    method {
                        method_name: name @filter(op: "=", value: ["$default_trait_method"]) @output

                        parameter {
                            position @output
                            param_name: name @output
                            normalized_type_signature {
                                param_signature: signature @output
                            }
                        }

                        return_value {
                            normalized_type_signature {
                                return_signature: signature @output
                            }
                        }
                    }
                }
            }
        }
    }
}
"#;
    let inherent_variables: BTreeMap<&str, FieldValue> = btreemap! {
        "inherent_owner" => "GenericExample".into(),
        "inherent_methods" => vec![FieldValue::String("combine".into()), FieldValue::String("pin_box_self".into())].into(),
    };
    let generic_pair_variables: BTreeMap<&str, FieldValue> = btreemap! {
        "inherent_owner" => "GenericPairExample".into(),
        "inherent_methods" => vec![FieldValue::String("parent_and_method".into())].into(),
    };
    let lifetime_const_variables: BTreeMap<&str, FieldValue> = btreemap! {
        "inherent_owner" => "LifetimeConstExample".into(),
        "inherent_methods" => vec![FieldValue::String("parent_lifetime_const".into())].into(),
    };
    let trait_variables = btreemap! {
        "trait_owner" => "GenericTrait",
        "trait_method" => "combine_trait",
    };
    let provider_trait_variables = btreemap! {
        "trait_owner" => "Provider",
        "trait_method" => "self_qualified",
    };
    let fn_output_trait_variables = btreemap! {
        "trait_owner" => "FnOutputTrait",
        "trait_method" => "fn_output_dyn",
    };
    let fn_output_trait_single_bound_variables = btreemap! {
        "trait_owner" => "FnOutputTrait",
        "trait_method" => "fn_output_dyn_single_bound",
    };
    let trait_impl_variables = btreemap! {
        "trait_owner" => "GenericTrait",
        "trait_method" => "combine_trait",
        "trait_impl_owner" => "ImplementsGenericTrait",
    };
    let default_trait_impl_variables = btreemap! {
        "default_trait_owner" => "DefaultGenericTrait",
        "default_trait_method" => "default_combine",
        "default_trait_impl_owner" => "UsesDefaultGenericTrait",
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        owner: String,
        method_name: String,
        position: u64,
        param_name: String,
        param_signature: String,
        return_signature: String,
    }

    let mut expected_results = vec![
        Output {
            owner: "GenericExample".into(),
            method_name: "combine".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "GenericExample".into(),
            method_name: "combine".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "TO1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "GenericExample".into(),
            method_name: "combine".into(),
            position: 3,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "GenericExample".into(),
            method_name: "pin_box_self".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "::core::pin::Pin<::alloc::boxed::Box<Self>>".into(),
            return_signature: "()".into(),
        },
        Output {
            owner: "GenericPairExample".into(),
            method_name: "parent_and_method".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature: "(TO1, TO2, T1)".into(),
        },
        Output {
            owner: "GenericPairExample".into(),
            method_name: "parent_and_method".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "TO1".into(),
            return_signature: "(TO1, TO2, T1)".into(),
        },
        Output {
            owner: "GenericPairExample".into(),
            method_name: "parent_and_method".into(),
            position: 3,
            param_name: "extra".into(),
            param_signature: "TO2".into(),
            return_signature: "(TO1, TO2, T1)".into(),
        },
        Output {
            owner: "GenericPairExample".into(),
            method_name: "parent_and_method".into(),
            position: 4,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(TO1, TO2, T1)".into(),
        },
        Output {
            owner: "LifetimeConstExample".into(),
            method_name: "parent_lifetime_const".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature: "(&'o1 [u8; CO1], T1)".into(),
        },
        Output {
            owner: "LifetimeConstExample".into(),
            method_name: "parent_lifetime_const".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "&'o1 [u8; CO1]".into(),
            return_signature: "(&'o1 [u8; CO1], T1)".into(),
        },
        Output {
            owner: "LifetimeConstExample".into(),
            method_name: "parent_lifetime_const".into(),
            position: 3,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(&'o1 [u8; CO1], T1)".into(),
        },
        Output {
            owner: "GenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "GenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "TO1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "GenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 3,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "Provider".into(),
            method_name: "self_qualified".into(),
            position: 1,
            param_name: "pair".into(),
            param_signature: "(Self::Assoc<IT1_1>, IT1_2)".into(),
            return_signature: "()".into(),
        },
        Output {
            owner: "FnOutputTrait".into(),
            method_name: "fn_output_dyn".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature:
                "impl ::core::clone::Clone + ::core::ops::function::Fn() -> (dyn ::core::marker::Send + ::core::marker::Sync)"
                    .into(),
        },
        Output {
            owner: "FnOutputTrait".into(),
            method_name: "fn_output_dyn_single_bound".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature:
                "impl ::core::ops::function::Fn() -> dyn ::core::marker::Send + ::core::marker::Sync"
                    .into(),
        },
        Output {
            owner: "ImplementsGenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&Self".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "ImplementsGenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "TO1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "ImplementsGenericTrait".into(),
            method_name: "combine_trait".into(),
            position: 3,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "UsesDefaultGenericTrait".into(),
            method_name: "default_combine".into(),
            position: 1,
            param_name: "self".into(),
            param_signature: "&mut Self".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "UsesDefaultGenericTrait".into(),
            method_name: "default_combine".into(),
            position: 2,
            param_name: "owner".into(),
            param_signature: "TO1".into(),
            return_signature: "(TO1, T1)".into(),
        },
        Output {
            owner: "UsesDefaultGenericTrait".into(),
            method_name: "default_combine".into(),
            position: 3,
            param_name: "method".into(),
            param_signature: "T1".into(),
            return_signature: "(TO1, T1)".into(),
        },
    ];
    expected_results.sort_unstable();

    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, adapter.clone(), inherent_query, inherent_variables)
            .expect("failed to run inherent method query")
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    inherent_query,
                    generic_pair_variables,
                )
                .expect("failed to run generic pair inherent method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    inherent_query,
                    lifetime_const_variables,
                )
                .expect("failed to run lifetime const inherent method query"),
            )
            .chain(
                trustfall::execute_query(&schema, adapter.clone(), trait_query, trait_variables)
                    .expect("failed to run trait method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    trait_query,
                    provider_trait_variables,
                )
                .expect("failed to run provider trait method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    trait_query,
                    fn_output_trait_variables,
                )
                .expect("failed to run FnOutputTrait method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    trait_query,
                    fn_output_trait_single_bound_variables,
                )
                .expect("failed to run FnOutputTrait single-bound method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    trait_impl_query,
                    trait_impl_variables,
                )
                .expect("failed to run trait impl method query"),
            )
            .chain(
                trustfall::execute_query(
                    &schema,
                    adapter.clone(),
                    default_trait_impl_query,
                    default_trait_impl_variables,
                )
                .expect("failed to run default trait impl method query"),
            )
            .map(|row| row.try_into_struct().expect("shape mismatch"))
            .collect();

    results.sort_unstable();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn function_parameter_normalized_type_signature_lint_shape() {
    get_test_data!(data, function_params_and_return_value);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Function {
                importable_path {
                    path @filter(op: "=", value: ["$path"])
                    public_api @filter(op: "=", value: ["$true"])
                }

                parameter {
                    position @filter(op: "=", value: ["$position"])
                    normalized_type_signature {
                        signature @output
                    }
                }
            }
        }
    }
}
"#;
    let variables = btreemap! {
        "path" => FieldValue::List(vec![
            FieldValue::String("function_params_and_return_value".into()),
            FieldValue::String("path_types".into()),
        ].into()),
        "position" => FieldValue::Uint64(2),
        "true" => FieldValue::Boolean(true),
    };

    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema failed to parse");

    #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
    struct Output {
        signature: String,
    }

    let expected_results = vec![Output {
        signature: "::alloc::vec::Vec<::function_params_and_return_value::PublicType<u8>>".into(),
    }];

    let results: Vec<Output> = trustfall::execute_query(&schema, adapter.clone(), query, variables)
        .expect("failed to run query")
        .map(|row| row.try_into_struct().expect("shape mismatch"))
        .collect();

    similar_asserts::assert_eq!(expected_results, results);
}

#[test]
fn rustdoc_trait_has_generic_associated_types() {
    get_test_data!(data, generic_associated_types);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on Trait {
                trait_name: name @output
                associated_type {
                    type_name: name @output
                    generic_parameter {
                        generic_name: name @output
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
        trait_name: String,
        type_name: String,
        generic_name: String,
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
                trait_name: "ConstGenericTrait".into(),
                type_name: "ConstTrait".into(),
                generic_name: "N".into(),
            },
            Output {
                trait_name: "LifetimeGenericTrait".into(),
                type_name: "Item".into(),
                generic_name: "'a".into(),
            },
            Output {
                trait_name: "TypeGenericTrait".into(),
                type_name: "Item".into(),
                generic_name: "T".into(),
            },
            Output {
                trait_name: "TypeLifetimeGenericTrait".into(),
                type_name: "Item".into(),
                generic_name: "'a".into(),
            },
            Output {
                trait_name: "TypeLifetimeGenericTrait".into(),
                type_name: "Item".into(),
                generic_name: "T".into(),
            },
        ],
        results
    );
}

#[test]
fn rustdoc_item_has_generic_associated_types() {
    get_test_data!(data, generic_associated_types);
    let adapter = RustdocAdapter::new(&data, None);
    let adapter = Arc::new(&adapter);

    let query = r#"
{
    Crate {
        item {
            ... on GenericItem {
                type_name: name @output
                generic_parameter {
                    generic_name: name @output
                    generic_kind: __typename @output
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
        type_name: String,
        generic_name: String,
        generic_kind: String,
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
                type_name: "ConstTrait".into(),
                generic_name: "N".into(),
                generic_kind: "GenericConstParameter".into(),
            },
            Output {
                type_name: "ConstTrait".into(),
                generic_name: "N".into(),
                generic_kind: "GenericConstParameter".into(),
            },
            Output {
                type_name: "Item".into(),
                generic_name: "'a".into(),
                generic_kind: "GenericLifetimeParameter".into(),
            },
            Output {
                type_name: "Item".into(),
                generic_name: "'a".into(),
                generic_kind: "GenericLifetimeParameter".into(),
            },
            Output {
                type_name: "Item".into(),
                generic_name: "T".into(),
                generic_kind: "GenericTypeParameter".into(),
            },
            Output {
                type_name: "Item".into(),
                generic_name: "T".into(),
                generic_kind: "GenericTypeParameter".into(),
            },
        ],
        results
    );
}
