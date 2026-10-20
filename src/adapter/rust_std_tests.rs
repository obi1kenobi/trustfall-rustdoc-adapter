//! Regression tests using the full definitions shipped in `rust-docs-json`.
//!
//! These tests use public import paths and observable behavior where possible.
//! Compiler size traits are identified by the canonical paths the adapter supports.
//! The blanket-impl witness checks its bounds explicitly so a change in Rust cannot
//! silently remove coverage of size-bound analysis.
//!
//! Install the component and opt in with `--cfg rust_std_tests`:
//! ```text
//! rustup component add --toolchain nightly rust-docs-json
//! RUSTFLAGS="--cfg rust_std_tests" \
//!   cargo +nightly test --locked --lib adapter::rust_std_tests::
//! ```
//!
//! The tests discover `core.json` in the active compiler's sysroot. Set `RUSTDOC_CORE_JSON`
//! to override that path, for example when testing JSON from another toolchain.

use std::{
    collections::BTreeMap,
    path::PathBuf,
    process::Command,
    sync::{Arc, LazyLock},
};

use rustdoc_types::{Crate, GenericBound, GenericParamDefKind, ItemEnum, ItemKind, Type};
use trustfall::{FieldValue, Schema, TryIntoStruct};

use crate::{
    PackageIndex, RustdocAdapter, hashtables::HashSet,
    sealed_trait::is_externally_satisfiable_blanket_impl_shim,
};

static CORE: LazyLock<Crate> = LazyLock::new(|| {
    let path = std::env::var_os("RUSTDOC_CORE_JSON")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
            let output = Command::new(rustc)
                .args(["--print", "sysroot"])
                .output()
                .expect("failed to run rustc --print sysroot; set RUSTDOC_CORE_JSON to override discovery");
            assert!(
                output.status.success(),
                "rustc --print sysroot failed with {}: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            );
            let sysroot = String::from_utf8(output.stdout)
                .expect("rustc --print sysroot returned a non-UTF-8 path");
            PathBuf::from(sysroot.trim()).join("share/doc/rust/json/core.json")
        });
    let content = std::fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!(
            "failed to read {}: {error}. Install rust-docs-json for the toolchain used \
             to run these tests, or set RUSTDOC_CORE_JSON to core.json",
            path.display()
        )
    });
    let format_version = crate::test_util::detect_rustdoc_format_version(&content)
        .expect("core.json is not rustdoc JSON");
    assert_eq!(
        format_version,
        rustdoc_types::FORMAT_VERSION,
        "incompatible rustdoc JSON format"
    );
    let rustdoc: Crate = serde_json::from_str(&content).expect("failed to parse core rustdoc JSON");
    let root = &rustdoc.index[&rustdoc.root];
    assert_eq!(
        root.name.as_deref(),
        Some("core"),
        "rustdoc JSON must describe core"
    );
    assert!(
        matches!(root.inner, ItemEnum::Module(_)),
        "core root must be a module"
    );
    rustdoc
});

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize)]
struct Output {
    path: Vec<String>,
    unconditionally_sealed: bool,
    public_api_sealed: bool,
}

fn trait_sealing_results(data: &PackageIndex<'_>, paths: &[[&str; 3]]) -> Vec<Output> {
    let adapter = RustdocAdapter::new(data, None);
    let schema =
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("failed to parse schema");
    let query = r#"
    {
        Crate {
            item {
                ... on Trait {
                    unconditionally_sealed @output
                    public_api_sealed @output
                    importable_path {
                        path @filter(op: "one_of", value: ["$paths"]) @output
                    }
                }
            }
        }
    }
    "#;
    let paths: Vec<FieldValue> = paths
        .iter()
        .map(|path| FieldValue::from(path.map(String::from).to_vec()))
        .collect();
    let variables = BTreeMap::from([("paths", FieldValue::from(paths))]);
    let mut results: Vec<Output> =
        trustfall::execute_query(&schema, Arc::new(&adapter), query, variables)
            .expect("failed to query core trait sealing")
            .map(|row| {
                row.try_into_struct()
                    .expect("trait sealing query returned an unexpected shape")
            })
            .collect();
    results.sort_unstable();
    results
}

/// Full size-trait definitions remain unconditionally sealed under both indexing policies.
#[test]
fn compiler_size_traits_are_sealed() {
    let expected = vec![
        Output {
            path: ["core", "marker", "MetaSized"].map(String::from).into(),
            unconditionally_sealed: true,
            public_api_sealed: true,
        },
        Output {
            path: ["core", "marker", "PointeeSized"].map(String::from).into(),
            unconditionally_sealed: true,
            public_api_sealed: true,
        },
        Output {
            path: ["core", "marker", "Sized"].map(String::from).into(),
            unconditionally_sealed: true,
            public_api_sealed: true,
        },
    ];
    for (indexing, data) in [
        ("ordinary", PackageIndex::from_crate(&CORE)),
        (
            "standard-library",
            PackageIndex::from_rust_std_component_crate(&CORE),
        ),
    ] {
        let results = trait_sealing_results(
            &data,
            &[
                ["core", "marker", "MetaSized"],
                ["core", "marker", "PointeeSized"],
                ["core", "marker", "Sized"],
            ],
        );
        similar_asserts::assert_eq!(expected, results, "{indexing} indexing");
    }
}

/// Size supertraits permit downstream implementations, including through `Copy: Clone: Sized`.
/// Standard-library instability must not prevent those implementations either.
#[test]
fn size_supertraits_do_not_seal_traits() {
    let expected = vec![
        Output {
            path: ["core", "clone", "Clone"].map(String::from).into(),
            unconditionally_sealed: false,
            public_api_sealed: false,
        },
        Output {
            path: ["core", "hash", "Hash"].map(String::from).into(),
            unconditionally_sealed: false,
            public_api_sealed: false,
        },
        Output {
            path: ["core", "marker", "Copy"].map(String::from).into(),
            unconditionally_sealed: false,
            public_api_sealed: false,
        },
    ];
    for (indexing, data) in [
        ("ordinary", PackageIndex::from_crate(&CORE)),
        (
            "standard-library",
            PackageIndex::from_rust_std_component_crate(&CORE),
        ),
    ] {
        let results = trait_sealing_results(
            &data,
            &[
                ["core", "clone", "Clone"],
                ["core", "hash", "Hash"],
                ["core", "marker", "Copy"],
            ],
        );
        similar_asserts::assert_eq!(expected, results, "{indexing} indexing");
    }
}

fn shared_reference_clone_impl(data: &PackageIndex<'_>) -> &'static rustdoc_types::Impl {
    let (clone_id, clone_trait) = CORE
        .index
        .iter()
        .filter_map(|(id, item)| match &item.inner {
            ItemEnum::Trait(trait_) => Some((id, trait_)),
            _ => None,
        })
        .find(|(id, _)| {
            data.own_crate
                .publicly_importable_names(id)
                .iter()
                .any(|path| path.path.components == ["core", "clone", "Clone"])
        })
        .expect("core::clone::Clone must have its full trait definition indexed");
    let implementations: Vec<_> = clone_trait
        .implementations
        .iter()
        .filter_map(|id| CORE.index.get(id))
        .filter_map(|item| match &item.inner {
            ItemEnum::Impl(impl_) if !impl_.is_negative => Some(impl_),
            _ => None,
        })
        .filter(|impl_| {
            matches!(
                &impl_.for_,
                Type::BorrowedRef { is_mutable: false, type_, .. }
                    if matches!(type_.as_ref(), Type::Generic(_))
            )
        })
        .collect();
    let [impl_] = implementations.as_slice() else {
        panic!("expected exactly one positive Clone impl for a shared reference to a generic type");
    };
    assert_eq!(
        impl_
            .trait_
            .as_ref()
            .expect("Clone impl must name its trait")
            .id,
        *clone_id
    );
    let Type::BorrowedRef { type_, .. } = &impl_.for_ else {
        unreachable!()
    };
    let Type::Generic(target_parameter) = type_.as_ref() else {
        unreachable!()
    };
    let parameter = impl_
        .generics
        .params
        .iter()
        .find(|parameter| parameter.name == *target_parameter)
        .expect("shared-reference Clone impl must declare its target parameter");
    let GenericParamDefKind::Type { bounds, .. } = &parameter.kind else {
        panic!("shared-reference Clone target parameter must be a type");
    };
    // This test must exercise an explicit size bound in the blanket analysis.
    // Accept any supported size trait so changes to Rust's size hierarchy do not
    // force a test update while the witness still covers the same behavior.
    let has_size_bound = bounds.iter().any(|bound| {
        let GenericBound::TraitBound {
            trait_,
            modifier: rustdoc_types::TraitBoundModifier::None,
            ..
        } = bound
        else {
            return false;
        };
        CORE.paths.get(&trait_.id).is_some_and(|item| {
            item.kind == ItemKind::Trait
                && matches!(
                    item.path.as_slice(),
                    [crate_name, module, name]
                        if crate_name == "core" && module == "marker"
                            && matches!(name.as_str(), "Sized" | "MetaSized" | "PointeeSized")
                )
        })
    });
    assert!(
        has_size_bound,
        "the shared-reference Clone impl no longer exercises a compiler size bound; \
         choose a new stdlib witness for size-bound blanket analysis"
    );
    impl_
}

/// Compiler size bounds on the real `Clone for &T` impl remain satisfiable under both indexing policies.
#[test]
fn size_bound_blanket_is_satisfiable() {
    for (indexing, data) in [
        ("ordinary", PackageIndex::from_crate(&CORE)),
        (
            "standard-library",
            PackageIndex::from_rust_std_component_crate(&CORE),
        ),
    ] {
        let impl_ = shared_reference_clone_impl(&data);
        let flags = data
            .own_crate
            .flags
            .as_ref()
            .expect("trait flags must be indexed");
        let unconditional_bounds_satisfiable = is_externally_satisfiable_blanket_impl_shim(
            &CORE.index,
            impl_,
            &mut flags.clone(),
            &mut HashSet::default(),
            false,
        );
        let public_api_bounds_satisfiable = is_externally_satisfiable_blanket_impl_shim(
            &CORE.index,
            impl_,
            &mut flags.clone(),
            &mut HashSet::default(),
            true,
        );
        assert!(
            unconditional_bounds_satisfiable,
            "compiler size bounds must permit downstream types ({indexing} indexing)"
        );
        assert!(
            public_api_bounds_satisfiable,
            "compiler size bounds must permit downstream types using public API ({indexing} indexing)"
        );
    }
}
