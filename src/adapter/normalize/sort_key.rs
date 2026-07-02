//! Canonical sort keys for unordered rustdoc type subtrees.
//!
//! Rustdoc preserves source/traversal order for semantically unordered groups,
//! such as trait bounds and associated-item constraints. These internal strings
//! let the formatter sort those groups before rendering them. Sort keys
//! normalize paths and generic names like final formatting does. These keys
//! are not user-facing normalized signatures.
//!
//! Parameter-position `impl Trait` needs a separate key mode because rustdoc
//! reports each use site separately from its synthetic generic param.
//! Parameter type signatures render those use sites as `IT...` placeholders,
//! so their nested bounds are erased from the enclosing key. Final output
//! renders the same nodes as opaque `impl ...` types, keeping their bounds
//! in the key. That avoids falling back to rustdoc/source order for otherwise
//! equal components.

use rustdoc_types::{
    AssocItemConstraint, AssocItemConstraintKind, FunctionSignature, GenericArg, GenericArgs,
    GenericBound, GenericParamDef, GenericParamDefKind, Path, Term, TraitBoundModifier, Type,
};

use crate::PackageIndex;

use super::{function_pointer, names::Names, paths};

#[derive(Clone, Copy)]
enum ImplTraitSortMode {
    /// Parameter sort keys render `impl Trait` use sites as `impl _`,
    /// because `impl Trait` parameters are normalized as synthetic generics
    /// whose bounds are reported separately on the synthetic generic itself.
    /// Including nested bounds would make sort order depend on details erased
    /// from the containing parameter type signature; see the module comment.
    EraseNestedBounds,
    /// Final-output sort keys include opaque `impl ...` bounds so that
    /// otherwise-equal components do not fall back to rustdoc/source order.
    IncludeNestedBounds,
}

/// Returns the canonical sort key for a poly-trait bound in parameter type mode.
pub(super) fn parameter_poly_trait<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    poly_trait: &'a rustdoc_types::PolyTrait,
) -> String {
    poly_trait_key(
        crate_,
        names,
        poly_trait,
        ImplTraitSortMode::EraseNestedBounds,
    )
}

/// Returns the canonical sort key for a poly-trait bound in final output mode.
pub(super) fn output_poly_trait<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    poly_trait: &'a rustdoc_types::PolyTrait,
) -> String {
    poly_trait_key(
        crate_,
        names,
        poly_trait,
        ImplTraitSortMode::IncludeNestedBounds,
    )
}

fn poly_trait_key<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    poly_trait: &'a rustdoc_types::PolyTrait,
    impl_trait_sort_mode: ImplTraitSortMode,
) -> String {
    let mut output = String::new();
    // TODO: If this cloning ends up being expensive,
    // we can look to replace it with an immutable hierarchical design instead.
    let mut scoped_names = names.clone();
    for param in &poly_trait.generic_params {
        scoped_names.add_higher_ranked_param(param);
    }
    format_generic_params(&scoped_names, &poly_trait.generic_params, &mut output);
    format_path(
        crate_,
        &scoped_names,
        &poly_trait.trait_,
        impl_trait_sort_mode,
        &mut output,
    );
    output
}

/// Returns the canonical sort key for an associated item constraint
/// in parameter type mode.
pub(super) fn parameter_assoc_item_constraint<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
) -> String {
    assoc_item_constraint_key(
        crate_,
        names,
        constraint,
        ImplTraitSortMode::EraseNestedBounds,
    )
}

/// Returns the canonical sort key for an associated item constraint
/// in final output mode.
pub(super) fn output_assoc_item_constraint<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
) -> String {
    assoc_item_constraint_key(
        crate_,
        names,
        constraint,
        ImplTraitSortMode::IncludeNestedBounds,
    )
}

fn assoc_item_constraint_key<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
    impl_trait_sort_mode: ImplTraitSortMode,
) -> String {
    let mut output = String::new();
    format_assoc_item_constraint(crate_, names, constraint, impl_trait_sort_mode, &mut output);
    output
}

/// Returns the canonical sort key for a generic bound in parameter type mode.
pub(super) fn parameter_generic_bound<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    bound: &'a GenericBound,
) -> String {
    generic_bound_key(crate_, names, bound, ImplTraitSortMode::EraseNestedBounds)
}

/// Returns the canonical sort key for a generic bound in final output mode.
pub(super) fn output_generic_bound<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    bound: &'a GenericBound,
) -> String {
    generic_bound_key(crate_, names, bound, ImplTraitSortMode::IncludeNestedBounds)
}

fn generic_bound_key<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    bound: &'a GenericBound,
    impl_trait_sort_mode: ImplTraitSortMode,
) -> String {
    let mut output = String::new();
    format_generic_bound(crate_, names, bound, impl_trait_sort_mode, &mut output);
    output
}

fn format_assoc_item_constraint<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    output.push_str(&constraint.name);
    if let Some(args) = constraint.args.as_deref() {
        format_generic_args(crate_, names, args, impl_trait_sort_mode, output);
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            output.push_str(": ");
            format_bounds(crate_, names, bounds, impl_trait_sort_mode, output);
        }
        AssocItemConstraintKind::Equality(term) => {
            output.push_str(" = ");
            format_term(crate_, names, term, impl_trait_sort_mode, output);
        }
    }
}

fn format_bounds<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    bounds: &'a [GenericBound],
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    let mut bounds = bounds
        .iter()
        .map(|bound| generic_bound_key(crate_, names, bound, impl_trait_sort_mode))
        .collect::<Vec<_>>();
    bounds.sort_unstable();
    let mut bounds_iter = bounds.iter();
    if let Some(bound) = bounds_iter.next() {
        output.push_str(bound);
    }
    for bound in bounds_iter {
        output.push_str(" + ");
        output.push_str(bound);
    }
}

fn format_generic_bound<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    bound: &'a GenericBound,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    match bound {
        GenericBound::TraitBound {
            trait_,
            generic_params,
            modifier,
        } => {
            // TODO: If this cloning ends up being expensive,
            // we can look to replace it with an immutable hierarchical design instead.
            let mut scoped_names = names.clone();
            for param in generic_params {
                scoped_names.add_higher_ranked_param(param);
            }
            format_generic_params(&scoped_names, generic_params, output);
            match modifier {
                TraitBoundModifier::None => {}
                TraitBoundModifier::Maybe => output.push('?'),
                // The final normalized signature hides nightly-only const-trait
                // markers, so canonical sort keys must ignore them too.
                TraitBoundModifier::MaybeConst => {}
            }
            format_path(crate_, &scoped_names, trait_, impl_trait_sort_mode, output);
        }
        GenericBound::Outlives(lifetime) => {
            let lifetime = names.lifetime(lifetime);
            output.push_str(lifetime.as_ref());
        }
        GenericBound::Use(args) => {
            output.push_str("use<");
            for (index, arg) in args.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                match arg {
                    rustdoc_types::PreciseCapturingArg::Lifetime(lifetime) => {
                        let lifetime = names.lifetime(lifetime);
                        output.push_str(lifetime.as_ref());
                    }
                    rustdoc_types::PreciseCapturingArg::Param(param) => {
                        let param = names.type_or_const_name(param);
                        output.push_str(param.as_ref());
                    }
                }
            }
            output.push('>');
        }
    }
}

fn format_path<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    path: &'a Path,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    output.push_str(&paths::normalized_path(crate_, path));
    if let Some(args) = path.args.as_deref() {
        format_generic_args(crate_, names, args, impl_trait_sort_mode, output);
    }
}

fn format_generic_args<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    args: &'a GenericArgs,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    match args {
        GenericArgs::AngleBracketed { args, constraints } => {
            if args.is_empty() && constraints.is_empty() {
                return;
            }

            output.push('<');
            let mut needs_separator = false;
            for arg in args {
                if needs_separator {
                    output.push_str(", ");
                }
                format_generic_arg(crate_, names, arg, impl_trait_sort_mode, output);
                needs_separator = true;
            }

            let mut constraints = constraints
                .iter()
                .map(|constraint| {
                    assoc_item_constraint_key(crate_, names, constraint, impl_trait_sort_mode)
                })
                .collect::<Vec<_>>();
            constraints.sort_unstable();
            for constraint in constraints {
                if needs_separator {
                    output.push_str(", ");
                }
                output.push_str(&constraint);
                needs_separator = true;
            }
            output.push('>');
        }
        GenericArgs::Parenthesized {
            inputs,
            output: return_type,
        } => {
            output.push('(');
            for (index, type_) in inputs.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                format_type(crate_, names, type_, impl_trait_sort_mode, output);
            }
            output.push(')');
            if let Some(return_type) = return_type {
                output.push_str(" -> ");
                format_type(crate_, names, return_type, impl_trait_sort_mode, output);
            }
        }
        GenericArgs::ReturnTypeNotation => output.push_str("(..)"),
    }
}

fn format_generic_arg<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    arg: &'a GenericArg,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    match arg {
        GenericArg::Lifetime(lifetime) => {
            let lifetime = names.lifetime(lifetime);
            output.push_str(lifetime.as_ref());
        }
        GenericArg::Type(type_) => {
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
        }
        GenericArg::Const(constant) => format_constant(names, constant, output),
        GenericArg::Infer => output.push('_'),
    }
}

fn format_term<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    term: &'a Term,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    match term {
        Term::Type(type_) => {
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
        }
        Term::Constant(constant) => {
            // As of Rust 1.96, associated const equality constraints such as
            // `T: Trait<N = 3>` are incomplete and rejected.
            // No sort key can be computed for such a term.
            unreachable!("found associated const equality constraint term: {constant:?}");
        }
    }
}

fn format_constant<'a>(
    names: &Names<'a>,
    constant: &'a rustdoc_types::Constant,
    output: &mut String,
) {
    let value = constant.value.as_deref().unwrap_or(&constant.expr);
    let value = names.const_expr(value);
    output.push_str(value.as_ref());
}

fn format_type<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    type_: &'a Type,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    match type_ {
        Type::ResolvedPath(path) => {
            format_path(crate_, names, path, impl_trait_sort_mode, output);
        }
        Type::DynTrait(dyn_trait) => {
            output.push_str("dyn ");
            let mut traits = dyn_trait
                .traits
                .iter()
                .map(|trait_| poly_trait_key(crate_, names, trait_, impl_trait_sort_mode))
                .collect::<Vec<_>>();
            traits.sort_unstable();
            let mut traits_iter = traits.iter();
            if let Some(trait_) = traits_iter.next() {
                output.push_str(trait_);
            }
            for trait_ in traits_iter {
                output.push_str(" + ");
                output.push_str(trait_);
            }
            if let Some(lifetime) = &dyn_trait.lifetime {
                assert!(
                    !traits.is_empty(),
                    "trait object lifetime bound cannot appear without a trait bound",
                );
                output.push_str(" + ");
                let lifetime = names.lifetime(lifetime);
                output.push_str(lifetime.as_ref());
            }
        }
        Type::Generic(name) => {
            let name = names.type_name(name);
            output.push_str(name.as_ref());
        }
        Type::Primitive(name) => output.push_str(name),
        Type::FunctionPointer(pointer) => {
            let mut scoped_names = names.clone();
            for param in &pointer.generic_params {
                scoped_names.add_higher_ranked_param(param);
            }
            format_generic_params(&scoped_names, &pointer.generic_params, output);
            function_pointer::format_fn_pointer_header(&pointer.header, output);
            output.push_str("fn");
            format_function_signature(
                crate_,
                &scoped_names,
                &pointer.sig,
                impl_trait_sort_mode,
                output,
            );
        }
        Type::Tuple(types) => {
            output.push('(');
            for (index, type_) in types.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                format_type(crate_, names, type_, impl_trait_sort_mode, output);
            }
            if types.len() == 1 {
                output.push(',');
            }
            output.push(')');
        }
        Type::Slice(type_) => {
            output.push('[');
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
            output.push(']');
        }
        Type::Array { type_, len } => {
            output.push('[');
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
            output.push_str("; ");
            let len = names.const_expr(len);
            output.push_str(len.as_ref());
            output.push(']');
        }
        Type::ImplTrait(bounds) => match impl_trait_sort_mode {
            ImplTraitSortMode::EraseNestedBounds => output.push_str("impl _"),
            ImplTraitSortMode::IncludeNestedBounds => {
                output.push_str("impl ");
                format_bounds(crate_, names, bounds, impl_trait_sort_mode, output);
            }
        },
        Type::Infer => output.push('_'),
        Type::RawPointer { is_mutable, type_ } => {
            if *is_mutable {
                output.push_str("*mut ");
            } else {
                output.push_str("*const ");
            }
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
        }
        Type::BorrowedRef {
            lifetime,
            is_mutable,
            type_,
        } => {
            output.push('&');
            if let Some(lifetime) = lifetime {
                let lifetime = names.lifetime(lifetime);
                output.push_str(lifetime.as_ref());
                output.push(' ');
            }
            if *is_mutable {
                output.push_str("mut ");
            }
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
        }
        Type::QualifiedPath {
            name,
            args,
            self_type,
            trait_,
        } => {
            output.push('<');
            format_type(crate_, names, self_type, impl_trait_sort_mode, output);
            if let Some(trait_) = trait_ {
                output.push_str(" as ");
                format_path(crate_, names, trait_, impl_trait_sort_mode, output);
            }
            output.push_str(">::");
            output.push_str(name);
            if let Some(args) = args.as_deref() {
                format_generic_args(crate_, names, args, impl_trait_sort_mode, output);
            }
        }
        Type::Pat { type_, .. } => {
            format_type(crate_, names, type_, impl_trait_sort_mode, output);
        }
    }
}

fn format_function_signature<'a>(
    crate_: &PackageIndex<'_>,
    names: &Names<'a>,
    signature: &'a FunctionSignature,
    impl_trait_sort_mode: ImplTraitSortMode,
    output: &mut String,
) {
    output.push('(');
    let mut needs_separator = false;
    for (_, type_) in &signature.inputs {
        if needs_separator {
            output.push_str(", ");
        }
        format_type(crate_, names, type_, impl_trait_sort_mode, output);
        needs_separator = true;
    }
    if signature.is_c_variadic {
        if needs_separator {
            output.push_str(", ");
        }
        output.push_str("...");
    }
    output.push(')');
    if let Some(return_type) = &signature.output {
        output.push_str(" -> ");
        format_type(crate_, names, return_type, impl_trait_sort_mode, output);
    }
}

fn format_generic_params<'a>(
    names: &Names<'a>,
    params: &'a [GenericParamDef],
    output: &mut String,
) {
    if params.is_empty() {
        return;
    }

    output.push_str("for<");
    for (index, param) in params.iter().enumerate() {
        if index != 0 {
            output.push_str(", ");
        }
        format_hrtb_generic_param_def(names, param, output);
    }
    output.push_str("> ");
}

fn format_hrtb_generic_param_def<'a>(
    names: &Names<'a>,
    param: &'a GenericParamDef,
    output: &mut String,
) {
    match &param.kind {
        GenericParamDefKind::Lifetime { .. } => {
            let lifetime = names.lifetime(&param.name);
            output.push_str(lifetime.as_ref());
        }
        GenericParamDefKind::Type { .. } => {
            // HRTB generic params model `for<...>` binders. As of Rust 1.96,
            // hypothetical `for<T>` binders are rejected.
            // No sort key can be computed for such a parameter.
            unreachable!("found type generic param definition in HRTB position: {param:?}");
        }
        GenericParamDefKind::Const { .. } => {
            // HRTB generic params model `for<...>` binders. As of Rust 1.96,
            // hypothetical `for<const N: usize>` binders are rejected.
            // No sort key can be computed for such a parameter.
            unreachable!("found const generic param definition in HRTB position: {param:?}");
        }
    }
}
