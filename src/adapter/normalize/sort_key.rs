//! Temporary canonical sort keys for rustdoc type subtrees.
//!
//! Parameter-position `impl Trait` placeholders are assigned before final type
//! formatting is possible. These helpers render enough of the relevant rustdoc
//! subtrees to sort bounds and constraints deterministically during that
//! precomputation. They are not user-facing normalized signatures.
//!
//! Sort keys deliberately use the same path and generic-name normalization as
//! final formatting. `Type::ImplTrait` nodes still render as `impl _` markers
//! because the enclosing associated item, generic arg, or bound determines
//! sort order; the nested `impl Trait` bounds are consumed separately for
//! placeholder assignment and do not affect that enclosing sort key. Skipping
//! those bounds here is a small optimization based on that invariant.

use rustdoc_types::{
    AssocItemConstraint, AssocItemConstraintKind, FunctionSignature, GenericArg, GenericArgs,
    GenericBound, GenericParamDef, GenericParamDefKind, Path, Term, TraitBoundModifier, Type,
};

use super::names::Names;

/// Returns the canonical sort key for a poly-trait bound.
pub(super) fn poly_trait<'a>(
    names: &Names<'a>,
    poly_trait: &'a rustdoc_types::PolyTrait,
    normalize_path: &impl Fn(&Path) -> String,
) -> String {
    let mut output = String::new();
    // TODO: If this cloning ends up being expensive,
    // we can look to replace it with an immutable hierarchical design instead.
    let mut scoped_names = names.clone();
    for param in &poly_trait.generic_params {
        scoped_names.add_param(param);
    }
    format_generic_params(&scoped_names, &poly_trait.generic_params, &mut output);
    format_path(
        &scoped_names,
        &poly_trait.trait_,
        normalize_path,
        &mut output,
    );
    output
}

/// Returns the canonical sort key for an associated item constraint.
pub(super) fn assoc_item_constraint<'a>(
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
    normalize_path: &impl Fn(&Path) -> String,
) -> String {
    let mut output = String::new();
    format_assoc_item_constraint(names, constraint, normalize_path, &mut output);
    output
}

/// Returns the canonical sort key for a generic bound.
pub(super) fn generic_bound<'a>(
    names: &Names<'a>,
    bound: &'a GenericBound,
    normalize_path: &impl Fn(&Path) -> String,
) -> String {
    let mut output = String::new();
    format_generic_bound(names, bound, normalize_path, &mut output);
    output
}

fn format_assoc_item_constraint<'a>(
    names: &Names<'a>,
    constraint: &'a AssocItemConstraint,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    output.push_str(&constraint.name);
    if let Some(args) = constraint.args.as_deref() {
        format_generic_args(names, args, normalize_path, output);
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            output.push_str(": ");
            format_bounds(names, bounds, normalize_path, output);
        }
        AssocItemConstraintKind::Equality(term) => {
            output.push_str(" = ");
            format_term(names, term, normalize_path, output);
        }
    }
}

fn format_bounds<'a>(
    names: &Names<'a>,
    bounds: &'a [GenericBound],
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    let mut bounds = bounds
        .iter()
        .map(|bound| generic_bound(names, bound, normalize_path))
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
    names: &Names<'a>,
    bound: &'a GenericBound,
    normalize_path: &impl Fn(&Path) -> String,
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
                scoped_names.add_param(param);
            }
            format_generic_params(&scoped_names, generic_params, output);
            match modifier {
                TraitBoundModifier::None => {}
                TraitBoundModifier::Maybe => output.push('?'),
                TraitBoundModifier::MaybeConst => output.push_str("~const "),
            }
            format_path(&scoped_names, trait_, normalize_path, output);
        }
        GenericBound::Outlives(lifetime) => {
            let lifetime = names.lifetime(lifetime);
            output.push_str(lifetime.as_ref());
        }
        GenericBound::Use(args) => {
            // Sort keys are used only while assigning parameter-position
            // `impl Trait` placeholders. As of Rust 1.96, hypothetical
            // `fn f(_: impl Trait + use<T>)` syntax is rejected, so precise
            // capture bounds cannot appear here.
            unreachable!(
                "found precise-capture `use<...>` bound in parameter-position impl Trait: {args:?}"
            );
        }
    }
}

fn format_path<'a>(
    names: &Names<'a>,
    path: &'a Path,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    output.push_str(&normalize_path(path));
    if let Some(args) = path.args.as_deref() {
        format_generic_args(names, args, normalize_path, output);
    }
}

fn format_generic_args<'a>(
    names: &Names<'a>,
    args: &'a GenericArgs,
    normalize_path: &impl Fn(&Path) -> String,
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
                format_generic_arg(names, arg, normalize_path, output);
                needs_separator = true;
            }

            let mut constraints = constraints
                .iter()
                .map(|constraint| assoc_item_constraint(names, constraint, normalize_path))
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
                format_type(names, type_, normalize_path, output);
            }
            output.push(')');
            if let Some(return_type) = return_type {
                output.push_str(" -> ");
                format_type(names, return_type, normalize_path, output);
            }
        }
        GenericArgs::ReturnTypeNotation => output.push_str("(..)"),
    }
}

fn format_generic_arg<'a>(
    names: &Names<'a>,
    arg: &'a GenericArg,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    match arg {
        GenericArg::Lifetime(lifetime) => {
            let lifetime = names.lifetime(lifetime);
            output.push_str(lifetime.as_ref());
        }
        GenericArg::Type(type_) => format_type(names, type_, normalize_path, output),
        GenericArg::Const(constant) => format_constant(names, constant, output),
        GenericArg::Infer => output.push('_'),
    }
}

fn format_term<'a>(
    names: &Names<'a>,
    term: &'a Term,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    match term {
        Term::Type(type_) => format_type(names, type_, normalize_path, output),
        Term::Constant(constant) => {
            // As of Rust 1.96, associated const equality constraints such as
            // `T: Trait<N = 3>` are incomplete and rejected, so there is no sort
            // key to compute for such a term.
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
    names: &Names<'a>,
    type_: &'a Type,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    match type_ {
        Type::ResolvedPath(path) => format_path(names, path, normalize_path, output),
        Type::DynTrait(dyn_trait) => {
            output.push_str("dyn ");
            let mut traits = dyn_trait
                .traits
                .iter()
                .map(|trait_| poly_trait(names, trait_, normalize_path))
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
                scoped_names.add_param(param);
            }
            format_generic_params(&scoped_names, &pointer.generic_params, output);
            output.push_str("fn");
            format_function_signature(&scoped_names, &pointer.sig, normalize_path, output);
        }
        Type::Tuple(types) => {
            output.push('(');
            for (index, type_) in types.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                format_type(names, type_, normalize_path, output);
            }
            if types.len() == 1 {
                output.push(',');
            }
            output.push(')');
        }
        Type::Slice(type_) => {
            output.push('[');
            format_type(names, type_, normalize_path, output);
            output.push(']');
        }
        Type::Array { type_, len } => {
            output.push('[');
            format_type(names, type_, normalize_path, output);
            output.push_str("; ");
            let len = names.const_expr(len);
            output.push_str(len.as_ref());
            output.push(']');
        }
        Type::ImplTrait(_) => {
            // Nested `impl Trait` bounds are consumed separately and receive
            // placeholders after the enclosing associated item, generic arg, or
            // bound has been placed in canonical order. Including those bounds
            // here would make sort order depend on details that are erased from
            // the containing parameter type signature, so `impl _` also avoids
            // formatting data that cannot affect ordering.
            output.push_str("impl _");
        }
        Type::Infer => output.push('_'),
        Type::RawPointer { is_mutable, type_ } => {
            if *is_mutable {
                output.push_str("*mut ");
            } else {
                output.push_str("*const ");
            }
            format_type(names, type_, normalize_path, output);
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
            format_type(names, type_, normalize_path, output);
        }
        Type::QualifiedPath {
            name,
            args,
            self_type,
            trait_,
        } => {
            output.push('<');
            format_type(names, self_type, normalize_path, output);
            if let Some(trait_) = trait_ {
                output.push_str(" as ");
                format_path(names, trait_, normalize_path, output);
            }
            output.push_str(">::");
            output.push_str(name);
            if let Some(args) = args.as_deref() {
                format_generic_args(names, args, normalize_path, output);
            }
        }
        Type::Pat { type_, .. } => format_type(names, type_, normalize_path, output),
    }
}

fn format_function_signature<'a>(
    names: &Names<'a>,
    signature: &'a FunctionSignature,
    normalize_path: &impl Fn(&Path) -> String,
    output: &mut String,
) {
    output.push('(');
    for (index, (_, type_)) in signature.inputs.iter().enumerate() {
        if index != 0 {
            output.push_str(", ");
        }
        format_type(names, type_, normalize_path, output);
    }
    output.push(')');
    if let Some(return_type) = &signature.output {
        output.push_str(" -> ");
        format_type(names, return_type, normalize_path, output);
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
            // hypothetical `for<T>` binders are rejected, so there is no sort
            // key to compute for such a parameter.
            unreachable!("found type generic param definition in HRTB position: {param:?}");
        }
        GenericParamDefKind::Const { .. } => {
            // HRTB generic params model `for<...>` binders. As of Rust 1.96,
            // hypothetical `for<const N: usize>` binders are rejected, so there
            // is no sort key to compute for such a parameter.
            unreachable!("found const generic param definition in HRTB position: {param:?}");
        }
    }
}
