//! Placeholder assignment for parameter-position `impl Trait`.
//!
//! Rustdoc splits each parameter-position `impl Trait` across two places: the
//! parameter type contains a `Type::ImplTrait` use site, while
//! `Function::generics.params` contains the corresponding synthetic generic
//! param and its bounds. Bounds and associated-item constraints are
//! semantically unordered, so equivalent source can appear in different rustdoc
//! orders.
//!
//! Canonical order is the deterministic order produced by sorting those
//! unordered pieces by normalized sort key. Formatter order is the order in
//! which `types.rs` encounters `Type::ImplTrait` nodes while rendering the raw
//! rustdoc type tree.
//!
//! This module uses both orders: a raw rustdoc-order walk pairs each use site
//! with the next synthetic generic param, while a canonical-order walk assigns
//! stable `IT<parameter>_<n>` placeholders. For example, in
//! `impl Trait<B = impl Copy, A = impl Clone>`, canonical order names `A`'s
//! `impl Clone` as `IT1_1` and `B`'s `impl Copy` as `IT1_2`, but the formatter
//! cursor yields `IT1_2` before `IT1_1` because it visits `B` before `A`.

use std::{borrow::Cow, collections::BTreeMap, num::NonZeroUsize};

use rustdoc_types::{
    AssocItemConstraint, AssocItemConstraintKind, GenericArg, GenericArgs, GenericBound,
    GenericParamDef, GenericParamDefKind, Generics, Path, Term, Type, WherePredicate,
};

use crate::PackageIndex;

use super::{
    names::{Names, is_synthetic_type_param, parameter_impl_trait_placeholder},
    paths, sort_key,
};

#[derive(Clone, Copy, Debug)]
struct SyntheticTypeParam {
    index: usize,
}

/// The `impl Trait` uses inside a function's parameters.
///
/// Each item inside `by_parameter` describes the `impl Trait` uses in
/// the corresponding parameter, matched positionally (by index).
/// When formatting, each parameter's data is consumed via [`ParameterImplTraitCursor`].
#[derive(Clone, Debug)]
pub(super) struct FnParameterImplTraits {
    by_parameter: Vec<Vec<Cow<'static, str>>>,
}

/// Cursor over the synthetic type-param names for one function parameter.
///
/// A parameter may contain multiple `impl Trait` occurrences, such as
/// `(&impl Clone, Vec<impl Into<String>>)`. During parameter formatting, each
/// `Type::ImplTrait` consumes the next name from this cursor. Return-position
/// `impl Trait` formatting does not use this cursor, so it remains an opaque
/// `impl` type rather than becoming a synthetic generic placeholder.
pub(super) struct ParameterImplTraitCursor<'a> {
    names: &'a [Cow<'static, str>],
    next: usize,
}

impl FnParameterImplTraits {
    /// Returns a cursor over placeholders consumed while formatting `position`.
    ///
    /// The cursor order matches the formatter traversal order of `Type::ImplTrait`
    /// nodes in that parameter. Placeholder assignment itself may have used a
    /// canonical ordering first, so callers should not index into
    /// `by_parameter` directly.
    pub(super) fn cursor_for_parameter(
        &self,
        position: NonZeroUsize,
    ) -> ParameterImplTraitCursor<'_> {
        let index = position.get() - 1;
        let names = self
            .by_parameter
            .get(index)
            .expect("function parameter position was out of bounds");
        ParameterImplTraitCursor { names, next: 0 }
    }
}

impl<'a> ParameterImplTraitCursor<'a> {
    pub(super) fn next(&mut self) -> &str {
        let name = self.names.get(self.next).unwrap_or_else(|| {
            unreachable!(
                "parameter-position impl Trait had no matching synthetic generic parameter: \
                next={}, names={:?}",
                self.next, self.names,
            )
        });
        self.next += 1;
        name.as_ref()
    }

    pub(super) fn assert_finished(&self) {
        assert_eq!(
            self.next,
            self.names.len(),
            "not all parameter-position impl Trait synthetic generic parameters were used",
        );
    }
}

fn collect_type_impl_trait_params<'a>(
    type_: &'a Type,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    match type_ {
        Type::ResolvedPath(path) => {
            collect_path_impl_trait_params(path, names, normalize_path, synthetic_params, output);
        }
        Type::DynTrait(dyn_trait) => {
            let mut traits = Vec::new();
            for trait_ in &dyn_trait.traits {
                assert_supported_hrtb_generic_params(&trait_.generic_params);
                // TODO: If this cloning ends up being expensive,
                // we can look to replace it with an immutable hierarchical design instead.
                let mut scoped_names = names.clone();
                for param in &trait_.generic_params {
                    scoped_names.add_param(param);
                }
                let mut params = Vec::new();
                collect_path_impl_trait_params(
                    &trait_.trait_,
                    &scoped_names,
                    normalize_path,
                    synthetic_params,
                    &mut params,
                );
                traits.push((sort_key::poly_trait(names, trait_, normalize_path), params));
            }
            traits.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            for (_, params) in traits {
                output.extend(params);
            }
        }
        Type::Generic(_) | Type::Primitive(_) | Type::Infer | Type::Pat { .. } => {
            // No `impl Trait` uses here.
        }
        Type::FunctionPointer(pointer) => {
            assert_supported_hrtb_generic_params(&pointer.generic_params);
            // TODO: If this cloning ends up being expensive,
            // we can look to replace it with an immutable hierarchical design instead.
            let mut scoped_names = names.clone();
            for param in &pointer.generic_params {
                scoped_names.add_param(param);
            }

            // As of Rust 1.96, `impl Trait` nested in `fn` pointer signatures is
            // rejected, so there are no parameter-position synthetic params to
            // collect inside `pointer.sig`.
        }
        Type::Tuple(types) => {
            for type_ in types {
                collect_type_impl_trait_params(
                    type_,
                    names,
                    normalize_path,
                    synthetic_params,
                    output,
                );
            }
        }
        Type::Slice(type_) | Type::Array { type_, .. } => {
            collect_type_impl_trait_params(type_, names, normalize_path, synthetic_params, output);
        }
        Type::ImplTrait(bounds) => {
            collect_bounds_impl_trait_params(
                bounds,
                names,
                normalize_path,
                synthetic_params,
                output,
            );
            output.push(next_synthetic_impl_trait_param(synthetic_params));
        }
        Type::RawPointer { type_, .. } | Type::BorrowedRef { type_, .. } => {
            collect_type_impl_trait_params(type_, names, normalize_path, synthetic_params, output);
        }
        Type::QualifiedPath {
            args,
            self_type,
            trait_,
            ..
        } => {
            collect_type_impl_trait_params(
                self_type,
                names,
                normalize_path,
                synthetic_params,
                output,
            );
            if let Some(trait_) = trait_ {
                collect_path_impl_trait_params(
                    trait_,
                    names,
                    normalize_path,
                    synthetic_params,
                    output,
                );
            }
            if let Some(args) = args.as_deref() {
                collect_generic_args_impl_trait_params(
                    args,
                    names,
                    normalize_path,
                    synthetic_params,
                    output,
                );
            }
        }
    }
}

fn assert_supported_hrtb_generic_params(params: &[GenericParamDef]) {
    for param in params {
        match &param.kind {
            GenericParamDefKind::Lifetime { .. } => {}
            GenericParamDefKind::Type { .. } => {
                // These generic params come from higher-ranked `for<...>`
                // binders on `fn` pointers or trait bounds. As of Rust 1.96,
                // hypothetical `for<T>` binders are rejected, so there are no
                // parameter-position synthetic params to collect here.
                unreachable!("found type generic param definition in HRTB position: {param:?}");
            }
            GenericParamDefKind::Const { .. } => {
                // These generic params come from higher-ranked `for<...>`
                // binders on `fn` pointers or trait bounds. As of Rust 1.96,
                // hypothetical `for<const N: usize>` binders are rejected, so
                // there are no parameter-position synthetic params to collect here.
                unreachable!("found const generic param definition in HRTB position: {param:?}");
            }
        }
    }
}

fn collect_path_impl_trait_params<'a>(
    path: &'a Path,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    if let Some(args) = path.args.as_deref() {
        collect_generic_args_impl_trait_params(
            args,
            names,
            normalize_path,
            synthetic_params,
            output,
        );
    }
}

fn collect_generic_args_impl_trait_params<'a>(
    args: &'a GenericArgs,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    match args {
        GenericArgs::AngleBracketed { args, constraints } => {
            for arg in args {
                match arg {
                    GenericArg::Type(type_) => {
                        collect_type_impl_trait_params(
                            type_,
                            names,
                            normalize_path,
                            synthetic_params,
                            output,
                        );
                    }
                    GenericArg::Lifetime(_) | GenericArg::Const(_) | GenericArg::Infer => {}
                }
            }

            let mut constraint_params = Vec::new();
            for constraint in constraints {
                let mut params = Vec::new();
                collect_assoc_item_constraint_impl_trait_params_raw(
                    constraint,
                    names,
                    normalize_path,
                    synthetic_params,
                    &mut params,
                );
                constraint_params.push((
                    sort_key::assoc_item_constraint(names, constraint, normalize_path),
                    params,
                ));
            }
            constraint_params.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            for (_, params) in constraint_params {
                output.extend(params);
            }
        }
        GenericArgs::Parenthesized { .. } => {
            // Parenthesized args represent `Fn`-trait signatures.
            // As of Rust 1.96, `impl Trait` nested in `Fn`-trait signatures is rejected,
            // so there are no parameter-position synthetic params to collect here.
        }
        GenericArgs::ReturnTypeNotation => {}
    }
}

fn collect_assoc_item_constraint_impl_trait_params_raw<'a>(
    constraint: &'a AssocItemConstraint,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    if let Some(args) = constraint.args.as_deref() {
        collect_generic_args_impl_trait_params(
            args,
            names,
            normalize_path,
            synthetic_params,
            output,
        );
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            collect_bounds_impl_trait_params(
                bounds,
                names,
                normalize_path,
                synthetic_params,
                output,
            );
        }
        AssocItemConstraintKind::Equality(term) => {
            collect_term_impl_trait_params(term, names, normalize_path, synthetic_params, output);
        }
    }
}

fn collect_bounds_impl_trait_params<'a>(
    bounds: &'a [GenericBound],
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    let mut bound_params = Vec::new();
    for bound in bounds {
        let mut params = Vec::new();
        match bound {
            GenericBound::TraitBound {
                trait_,
                generic_params,
                ..
            } => {
                assert_supported_hrtb_generic_params(generic_params);
                // TODO: If this cloning ends up being expensive,
                // we can look to replace it with an immutable hierarchical design instead.
                let mut scoped_names = names.clone();
                for param in generic_params {
                    scoped_names.add_param(param);
                }
                collect_path_impl_trait_params(
                    trait_,
                    &scoped_names,
                    normalize_path,
                    synthetic_params,
                    &mut params,
                );
            }
            GenericBound::Outlives(_) | GenericBound::Use(_) => {}
        }
        bound_params.push((
            sort_key::generic_bound(names, bound, normalize_path),
            params,
        ));
    }

    bound_params.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    for (_, params) in bound_params {
        output.extend(params);
    }
}

fn collect_term_impl_trait_params<'a>(
    term: &'a Term,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    output: &mut Vec<SyntheticTypeParam>,
) {
    match term {
        Term::Type(type_) => {
            collect_type_impl_trait_params(type_, names, normalize_path, synthetic_params, output);
        }
        Term::Constant(constant) => {
            // As of Rust 1.96, associated const equality constraints such as
            // `T: Trait<N = 3>` are incomplete and rejected, so there are no
            // parameter-position synthetic params to collect here.
            unreachable!("found associated const equality constraint term: {constant:?}");
        }
    }
}

fn next_synthetic_impl_trait_param(
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
) -> SyntheticTypeParam {
    synthetic_params
        .next()
        .expect("parameter-position impl Trait had no matching synthetic generic parameter")
}

/// Computes the parameter-position `impl Trait` placeholders for one function.
///
/// Placeholder assignment is scoped to each containing function parameter:
/// `impl Trait` in the first parameter is named `IT1_1`, `IT1_2`, and so on,
/// while `impl Trait` in the second parameter is named `IT2_1`, `IT2_2`, and so
/// on. This keeps unrelated parameter positions from being renumbered when a
/// new `impl Trait` is added elsewhere in the same function signature.
pub(super) fn compute_for_function<'a>(
    crate_: &'a PackageIndex<'a>,
    function: &'a rustdoc_types::Function,
    names: &Names<'a>,
) -> FnParameterImplTraits {
    let normalize_path = |path: &Path| paths::normalized_path(crate_, path);
    compute_for_function_with_path_normalizer(function, names, &normalize_path)
}

fn compute_for_function_with_path_normalizer<'a>(
    function: &'a rustdoc_types::Function,
    names: &Names<'a>,
    normalize_path: &impl Fn(&Path) -> String,
) -> FnParameterImplTraits {
    let synthetic_params = function
        .generics
        .params
        .iter()
        .enumerate()
        .filter(|(_, param)| is_synthetic_type_param(param))
        .map(|(index, _)| SyntheticTypeParam { index })
        .collect::<Vec<_>>();
    let mut synthetic_params = synthetic_params.iter().copied();
    let mut sink = Vec::new();

    collect_generics_impl_trait_names(
        &function.generics,
        &mut synthetic_params,
        None,
        &mut sink,
        false,
    );

    let mut output = Vec::with_capacity(function.sig.inputs.len());
    for (index, (_, type_)) in function.sig.inputs.iter().enumerate() {
        let parameter_position = index + 1;
        // Assign each parameter's suffixes in canonical order, but leave the
        // real cursor untouched. The formatter walks raw rustdoc nodes while
        // building strings that may later be sorted, so the cursor it receives
        // must remain in formatter traversal order.
        let mut canonical_synthetic_params = synthetic_params.clone();
        let mut canonical_params = Vec::new();
        collect_type_impl_trait_params(
            type_,
            names,
            normalize_path,
            &mut canonical_synthetic_params,
            &mut canonical_params,
        );

        let mut synthetic_names_by_index = BTreeMap::new();
        for (local_index, param) in canonical_params.iter().enumerate() {
            let existing = synthetic_names_by_index.insert(
                param.index,
                parameter_impl_trait_placeholder(parameter_position, local_index + 1),
            );
            assert!(
                existing.is_none(),
                "duplicate synthetic type parameter index `{}` in parameter {parameter_position}",
                param.index,
            );
        }

        let mut parameter_names = Vec::new();
        collect_type_impl_trait_names(
            type_,
            &mut synthetic_params,
            Some(&synthetic_names_by_index),
            &mut parameter_names,
            true,
        );
        output.push(parameter_names);
    }

    collect_where_predicates_impl_trait_names(
        &function.generics.where_predicates,
        &mut synthetic_params,
        None,
        &mut sink,
        false,
    );

    assert!(
        synthetic_params.next().is_none(),
        "rustdoc synthetic generics outnumbered parameter-position impl Trait occurrences",
    );
    FnParameterImplTraits {
        by_parameter: output,
    }
}

fn collect_generics_impl_trait_names(
    generics: &Generics,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    for param in &generics.params {
        match &param.kind {
            GenericParamDefKind::Type {
                bounds,
                default,
                is_synthetic: false,
            } => {
                collect_bounds_impl_trait_names(
                    bounds,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
                if let Some(default) = default {
                    collect_type_impl_trait_names(
                        default,
                        synthetic_params,
                        synthetic_names_by_index,
                        output,
                        emit,
                    );
                }
            }
            GenericParamDefKind::Type {
                is_synthetic: true, ..
            }
            | GenericParamDefKind::Lifetime { .. } => {}
            GenericParamDefKind::Const { type_, .. } => {
                collect_type_impl_trait_names(
                    type_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
        }
    }
}

fn collect_type_impl_trait_names(
    type_: &Type,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match type_ {
        Type::ResolvedPath(path) => {
            collect_path_impl_trait_names(
                path,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
        Type::DynTrait(dyn_trait) => {
            for trait_ in &dyn_trait.traits {
                assert_supported_hrtb_generic_params(&trait_.generic_params);
                collect_path_impl_trait_names(
                    &trait_.trait_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
        }
        Type::Generic(_) | Type::Primitive(_) | Type::Infer | Type::Pat { .. } => {}
        Type::FunctionPointer(pointer) => {
            assert_supported_hrtb_generic_params(&pointer.generic_params);
            // As of Rust 1.96, `impl Trait` nested in `fn` pointer signatures is
            // rejected, so there are no parameter-position synthetic params to
            // collect inside `pointer.sig`.
        }
        Type::Tuple(types) => {
            for type_ in types {
                collect_type_impl_trait_names(
                    type_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
        }
        Type::Slice(type_) | Type::Array { type_, .. } => {
            collect_type_impl_trait_names(
                type_,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
        Type::ImplTrait(bounds) => {
            collect_bounds_impl_trait_names(
                bounds,
                synthetic_params,
                synthetic_names_by_index,
                output,
                false,
            );
            let param = next_synthetic_impl_trait_param(synthetic_params);
            if emit {
                let names = synthetic_names_by_index.expect(
                    "parameter-position impl Trait name emission requires precomputed names",
                );
                output.push(names.get(&param.index).cloned().unwrap_or_else(|| {
                    unreachable!(
                        "missing normalized name for synthetic type parameter index `{}`",
                        param.index,
                    )
                }));
            }
        }
        Type::RawPointer { type_, .. } | Type::BorrowedRef { type_, .. } => {
            collect_type_impl_trait_names(
                type_,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
        Type::QualifiedPath {
            args,
            self_type,
            trait_,
            ..
        } => {
            collect_type_impl_trait_names(
                self_type,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
            if let Some(trait_) = trait_ {
                collect_path_impl_trait_names(
                    trait_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
            if let Some(args) = args.as_deref() {
                collect_generic_args_impl_trait_names(
                    args,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
        }
    }
}

fn collect_path_impl_trait_names(
    path: &Path,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    if let Some(args) = path.args.as_deref() {
        collect_generic_args_impl_trait_names(
            args,
            synthetic_params,
            synthetic_names_by_index,
            output,
            emit,
        );
    }
}

fn collect_generic_args_impl_trait_names(
    args: &GenericArgs,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match args {
        GenericArgs::AngleBracketed { args, constraints } => {
            for arg in args {
                match arg {
                    GenericArg::Type(type_) => {
                        collect_type_impl_trait_names(
                            type_,
                            synthetic_params,
                            synthetic_names_by_index,
                            output,
                            emit,
                        );
                    }
                    GenericArg::Lifetime(_) | GenericArg::Const(_) | GenericArg::Infer => {}
                }
            }
            for constraint in constraints {
                collect_assoc_item_constraint_impl_trait_names(
                    constraint,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
        }
        GenericArgs::Parenthesized { .. } => {
            // Parenthesized args represent `Fn`-trait signatures.
            // As of Rust 1.96, `impl Trait` nested in those input or output
            // types is rejected, so there are no parameter-position synthetic
            // params to collect here.
        }
        GenericArgs::ReturnTypeNotation => {}
    }
}

fn collect_assoc_item_constraint_impl_trait_names(
    constraint: &AssocItemConstraint,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    if let Some(args) = constraint.args.as_deref() {
        collect_generic_args_impl_trait_names(
            args,
            synthetic_params,
            synthetic_names_by_index,
            output,
            emit,
        );
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            collect_bounds_impl_trait_names(
                bounds,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
        AssocItemConstraintKind::Equality(term) => {
            collect_term_impl_trait_names(
                term,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
    }
}

fn collect_bounds_impl_trait_names(
    bounds: &[GenericBound],
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    for bound in bounds {
        match bound {
            GenericBound::TraitBound {
                trait_,
                generic_params,
                ..
            } => {
                assert_supported_hrtb_generic_params(generic_params);
                collect_path_impl_trait_names(
                    trait_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
            GenericBound::Outlives(_) | GenericBound::Use(_) => {}
        }
    }
}

fn collect_term_impl_trait_names(
    term: &Term,
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match term {
        Term::Type(type_) => {
            collect_type_impl_trait_names(
                type_,
                synthetic_params,
                synthetic_names_by_index,
                output,
                emit,
            );
        }
        Term::Constant(constant) => {
            // As of Rust 1.96, associated const equality constraints such as
            // `T: Trait<N = 3>` are incomplete and rejected, so there are no
            // parameter-position synthetic params to collect here.
            unreachable!("found associated const equality constraint term: {constant:?}");
        }
    }
}

fn collect_where_predicates_impl_trait_names(
    predicates: &[WherePredicate],
    synthetic_params: &mut impl Iterator<Item = SyntheticTypeParam>,
    synthetic_names_by_index: Option<&BTreeMap<usize, Cow<'static, str>>>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    for predicate in predicates {
        match predicate {
            WherePredicate::BoundPredicate {
                type_,
                bounds,
                generic_params,
            } => {
                assert_supported_hrtb_generic_params(generic_params);
                collect_type_impl_trait_names(
                    type_,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
                collect_bounds_impl_trait_names(
                    bounds,
                    synthetic_params,
                    synthetic_names_by_index,
                    output,
                    emit,
                );
            }
            WherePredicate::LifetimePredicate { .. } => {}
            WherePredicate::EqPredicate { .. } => {
                // As of Rust 1.96, general equality constraints such as
                // `where T::Assoc = U` are rejected. Associated-item
                // constraints such as `where T: Trait<Assoc = U>` are represented
                // as `BoundPredicate` instead, so there are no
                // parameter-position synthetic params to collect here.
                unreachable!(
                    "found general equality predicate in function generics: {predicate:?}"
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::adapter::normalize::names::parameter_impl_trait_placeholder;

    #[test]
    fn first_impl_trait_names_use_requested_static_prefix() {
        assert!(matches!(
            parameter_impl_trait_placeholder(1, 1),
            Cow::Borrowed("IT1_1")
        ));
        assert!(matches!(
            parameter_impl_trait_placeholder(8, 1),
            Cow::Borrowed("IT8_1")
        ));
        assert!(
            matches!(parameter_impl_trait_placeholder(9, 1), Cow::Owned(value) if value == "IT9_1")
        );
        assert!(
            matches!(parameter_impl_trait_placeholder(1, 2), Cow::Owned(value) if value == "IT1_2")
        );
    }
}
