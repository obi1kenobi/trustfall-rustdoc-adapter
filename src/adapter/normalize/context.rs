use std::{borrow::Cow, num::NonZeroUsize};

use rustdoc_types::{
    AssocItemConstraint, AssocItemConstraintKind, FunctionSignature, GenericArg, GenericArgs,
    GenericBound, GenericParamDef, GenericParamDefKind, ItemEnum, Path, Term, Type,
};

use crate::{PackageIndex, adapter::vertex::FunctionContext};

use super::names::Names;

/// Per-function state used while rendering one normalized signature.
///
/// A context is built for the function item that owns the parameter or return
/// value currently being rendered. It includes generics from the containing
/// trait or `impl` when there is one, but it is not shared across all methods in
/// an `impl` block: each function has its own rustdoc signature and its own
/// synthetic generic parameters.
#[derive(Clone)]
pub(super) struct FnNormalizationContext<'a> {
    crate_: &'a PackageIndex<'a>,
    names: Names<'a>,

    /// Outer vec: one slot per function parameter, indexed by 0-based parameter.
    /// Inner vec: the normalized names for the visible parameter-position
    /// `impl Trait` nodes inside that parameter's type, in the
    /// same preorder recursive traversal the type formatter walks them.
    ///
    /// For example, in `fn f<T>(a: T, b: &impl Clone, c: (impl Default, Vec<impl Into<String>>))`,
    /// the function-level type counter assigns `T1`, `IT2`, `IT3`, and `IT4`.
    /// This field then holds `[[], [IT2], [IT3, IT4]]`: parameter `a` has no
    /// `impl Trait`, parameter `b` has one, and parameter `c` has two.
    ///
    /// Bounds inside an `impl Trait` belong to that synthetic type parameter, not
    /// to the normalized signature of the containing parameter type. In
    /// `fn g(value: impl Iterator<Item = impl Into<String>>)`, we create
    /// `IT1` for the nested `impl Into<String>` and `IT2` for the outer
    /// `impl Iterator<...>`. However, `value`'s type signature is just `IT2`.
    ///
    /// Rustdoc represents parameter-position `impl Trait` in two separate places:
    /// the parameter type tree contains `Type::ImplTrait`, while the function's
    /// generic parameter list contains the corresponding synthetic type params.
    /// There is no ID or reference connecting those two representations. The
    /// shared invariant we can rely on is rustdoc's traversal order, so we
    /// pre-partition the synthetic names by parameter and give the formatter a
    /// cursor over the relevant inner vec.
    ///
    /// The values are `Cow<'static, str>` because common names such as `IT1`
    /// through `IT8` are borrowed statics; larger counters fall back to owned
    /// strings.
    parameter_impl_trait_names: Vec<Vec<Cow<'static, str>>>,
}

/// Cursor over the synthetic type-param names for one function parameter.
///
/// A parameter may contain multiple `impl Trait` occurrences, such as
/// `(&impl Clone, Vec<impl Into<String>>)`. During parameter formatting, each
/// `Type::ImplTrait` consumes the next name from this cursor. Return-position
/// `impl Trait` formatting does not use this cursor, so it remains an opaque
/// `impl` type rather than becoming `ITn`.
pub(super) struct ParameterImplTraitNames<'a> {
    names: &'a [Cow<'static, str>],
    next: usize,
}

impl<'a> ParameterImplTraitNames<'a> {
    pub(super) fn next(&mut self) -> &str {
        let name = self.names.get(self.next).unwrap_or_else(|| {
            unreachable!(
                "parameter-position impl Trait had no matching synthetic generic parameter"
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

impl<'a> FnNormalizationContext<'a> {
    pub(super) fn new(crate_: &'a PackageIndex<'a>, fn_ctx: FunctionContext<'a>) -> Self {
        let mut names = Names::default();

        // Parent generics must be introduced before function generics so
        // normalized numbering follows the source-visible outer-to-inner scope.
        if let Some(item) = fn_ctx.parent {
            match &item.inner {
                ItemEnum::Trait(trait_) => {
                    names.add_params(&trait_.generics.params);
                }
                ItemEnum::Impl(impl_) => {
                    names.add_params(&impl_.generics.params);
                }
                _ => unreachable!("function parent was not a trait or impl: {item:?}"),
            }
        }

        let ItemEnum::Function(function_inner) = &fn_ctx.function.inner else {
            unreachable!("normalized type signatures require a function item");
        };
        names.add_params(&function_inner.generics.params);
        let parameter_impl_trait_names = parameter_impl_trait_names(
            &function_inner.sig.inputs,
            &function_inner.generics.params,
            &names,
        );

        Self {
            crate_,
            names,
            parameter_impl_trait_names,
        }
    }

    pub(super) fn with_params(&self, params: &'a [rustdoc_types::GenericParamDef]) -> Self {
        // HRTB and function-pointer binders add a nested scope. Clone the
        // context so outer generic mappings remain available while nested
        // parameters get fresh canonical names.
        // TODO: If this cloning becomes a bottleneck, we can probably design
        // a hierarchical name structure, since we don't expect deep levels of nesting.
        let mut value = self.clone();
        value.names.add_params(params);
        value
    }

    pub(super) fn crate_(&self) -> &'a PackageIndex<'a> {
        self.crate_
    }

    pub(super) fn names(&self) -> &Names<'a> {
        &self.names
    }

    /// Get the synthetic generic names for a parameter's `impl Trait` nodes.
    ///
    /// Rustdoc stores each argument-position `impl Trait` occurrence twice: as a
    /// `Type::ImplTrait` node at its input position, and as a synthetic generic
    /// parameter in the containing function's generics. Rustdoc does not link
    /// those two representations directly, so we rely on their shared traversal
    /// order within function inputs. Return-position `impl Trait` is formatted
    /// without this cursor and therefore remains an opaque `impl` type.
    pub(super) fn parameter_impl_trait_names(
        &self,
        position: NonZeroUsize,
    ) -> ParameterImplTraitNames<'_> {
        let index = position.get() - 1;
        let names = self
            .parameter_impl_trait_names
            .get(index)
            .expect("function parameter position was out of bounds");
        ParameterImplTraitNames { names, next: 0 }
    }
}

/// Build the per-parameter cursor data from rustdoc's synthetic generics.
fn parameter_impl_trait_names<'a>(
    inputs: &'a [(String, Type)],
    params: &'a [GenericParamDef],
    names: &Names<'a>,
) -> Vec<Vec<Cow<'static, str>>> {
    let mut synthetic_params = params.iter().filter(|param| {
        matches!(
            param.kind,
            GenericParamDefKind::Type {
                is_synthetic: true,
                ..
            }
        )
    });

    let mut output = Vec::with_capacity(inputs.len());
    for (_, type_) in inputs {
        let mut parameter_names = Vec::new();
        collect_type_impl_trait_names(
            type_,
            &mut synthetic_params,
            names,
            &mut parameter_names,
            true,
        );
        output.push(parameter_names);
    }

    assert!(
        synthetic_params.next().is_none(),
        "rustdoc synthetic generic parameters outnumbered parameter-position impl Trait occurrences",
    );
    output
}

fn collect_type_impl_trait_names<'a>(
    type_: &'a Type,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match type_ {
        Type::ResolvedPath(path) => {
            collect_path_impl_trait_names(path, synthetic_params, names, output, emit);
        }
        Type::DynTrait(dyn_trait) => {
            for trait_ in &dyn_trait.traits {
                collect_generic_params_impl_trait_names(
                    &trait_.generic_params,
                    synthetic_params,
                    names,
                    output,
                    emit,
                );
                collect_path_impl_trait_names(
                    &trait_.trait_,
                    synthetic_params,
                    names,
                    output,
                    emit,
                );
            }
        }
        Type::Generic(_) | Type::Primitive(_) | Type::Infer | Type::Pat { .. } => {}
        Type::FunctionPointer(pointer) => {
            collect_generic_params_impl_trait_names(
                &pointer.generic_params,
                synthetic_params,
                names,
                output,
                emit,
            );
            collect_function_signature_impl_trait_names(
                &pointer.sig,
                synthetic_params,
                names,
                output,
                emit,
            );
        }
        Type::Tuple(types) => {
            for type_ in types {
                collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
            }
        }
        Type::Slice(type_) | Type::Array { type_, .. } => {
            collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
        }
        Type::ImplTrait(bounds) => {
            collect_bounds_impl_trait_names(bounds, synthetic_params, names, output, false);
            let name = next_synthetic_impl_trait_name(synthetic_params, names);
            if emit {
                output.push(name);
            }
        }
        Type::RawPointer { type_, .. } | Type::BorrowedRef { type_, .. } => {
            collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
        }
        Type::QualifiedPath {
            args,
            self_type,
            trait_,
            ..
        } => {
            collect_type_impl_trait_names(self_type, synthetic_params, names, output, emit);
            if let Some(trait_) = trait_ {
                collect_path_impl_trait_names(trait_, synthetic_params, names, output, emit);
            }
            if let Some(args) = args.as_deref() {
                collect_generic_args_impl_trait_names(args, synthetic_params, names, output, emit);
            }
        }
    }
}

fn collect_generic_params_impl_trait_names<'a>(
    params: &'a [GenericParamDef],
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    for param in params {
        match &param.kind {
            GenericParamDefKind::Const { type_, .. } => {
                collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
            }
            GenericParamDefKind::Lifetime { .. } | GenericParamDefKind::Type { .. } => {}
        }
    }
}

fn collect_path_impl_trait_names<'a>(
    path: &'a Path,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    if let Some(args) = path.args.as_deref() {
        collect_generic_args_impl_trait_names(args, synthetic_params, names, output, emit);
    }
}

fn collect_generic_args_impl_trait_names<'a>(
    args: &'a GenericArgs,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match args {
        GenericArgs::AngleBracketed { args, constraints } => {
            for arg in args {
                match arg {
                    GenericArg::Type(type_) => {
                        collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
                    }
                    GenericArg::Lifetime(_) | GenericArg::Const(_) | GenericArg::Infer => {}
                }
            }
            for constraint in constraints {
                collect_assoc_item_constraint_impl_trait_names(
                    constraint,
                    synthetic_params,
                    names,
                    output,
                    emit,
                );
            }
        }
        GenericArgs::Parenthesized {
            inputs,
            output: return_type,
        } => {
            for type_ in inputs {
                collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
            }
            if let Some(return_type) = return_type {
                collect_type_impl_trait_names(return_type, synthetic_params, names, output, emit);
            }
        }
        GenericArgs::ReturnTypeNotation => {}
    }
}

fn collect_assoc_item_constraint_impl_trait_names<'a>(
    constraint: &'a AssocItemConstraint,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    if let Some(args) = constraint.args.as_deref() {
        collect_generic_args_impl_trait_names(args, synthetic_params, names, output, emit);
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            collect_bounds_impl_trait_names(bounds, synthetic_params, names, output, emit);
        }
        AssocItemConstraintKind::Equality(term) => {
            collect_term_impl_trait_names(term, synthetic_params, names, output, emit);
        }
    }
}

fn collect_bounds_impl_trait_names<'a>(
    bounds: &'a [GenericBound],
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
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
                collect_generic_params_impl_trait_names(
                    generic_params,
                    synthetic_params,
                    names,
                    output,
                    emit,
                );
                collect_path_impl_trait_names(trait_, synthetic_params, names, output, emit);
            }
            GenericBound::Outlives(_) | GenericBound::Use(_) => {}
        }
    }
}

fn collect_term_impl_trait_names<'a>(
    term: &'a Term,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    match term {
        Term::Type(type_) => {
            collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
        }
        Term::Constant(_) => {}
    }
}

fn collect_function_signature_impl_trait_names<'a>(
    signature: &'a FunctionSignature,
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
    output: &mut Vec<Cow<'static, str>>,
    emit: bool,
) {
    for (_, type_) in &signature.inputs {
        collect_type_impl_trait_names(type_, synthetic_params, names, output, emit);
    }
    if let Some(return_type) = &signature.output {
        collect_type_impl_trait_names(return_type, synthetic_params, names, output, emit);
    }
}

fn next_synthetic_impl_trait_name<'a>(
    synthetic_params: &mut impl Iterator<Item = &'a GenericParamDef>,
    names: &Names<'a>,
) -> Cow<'static, str> {
    let param = synthetic_params
        .next()
        .expect("parameter-position impl Trait had no matching synthetic generic parameter");
    names.type_name(&param.name)
}
