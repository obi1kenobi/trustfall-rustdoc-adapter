use std::num::NonZeroUsize;

use rustdoc_types::{
    Abi, AssocItemConstraint, AssocItemConstraintKind, FunctionHeader, GenericArg, GenericBound,
    GenericParamDef, GenericParamDefKind, Term, TraitBoundModifier, Type,
};

use super::{
    context::FnNormalizationContext, parameter_impl_trait::ParameterImplTraitCursor, paths,
};

/// Output a normalized parameter type.
///
/// In parameters, `impl Trait` is normalized to a synthetic generic type, not an opaque type.
pub(super) fn format_parameter_type<'a>(
    context: &FnNormalizationContext<'a>,
    position: NonZeroUsize,
    type_: &'a Type,
) -> String {
    let mut parameter_impl_trait_cursor = Some(context.impl_trait_cursor_for_parameter(position));
    let mut output = String::new();
    format_type_inner(
        context,
        type_,
        false,
        &mut parameter_impl_trait_cursor,
        &mut output,
    );
    parameter_impl_trait_cursor
        .as_ref()
        .expect("parameter impl Trait cursor disappeared")
        .assert_finished();
    output
}

/// Output a normalized type for positions other than parameter position.
///
/// In non-parameters, `impl Trait` is normalized to an opaque type, not a synthetic generic type.
pub(super) fn format_type<'a>(context: &FnNormalizationContext<'a>, type_: &'a Type) -> String {
    let mut parameter_impl_trait_cursor = None;
    let mut output = String::new();
    format_type_inner(
        context,
        type_,
        false,
        &mut parameter_impl_trait_cursor,
        &mut output,
    );
    output
}

fn format_type_inner<'a>(
    context: &FnNormalizationContext<'a>,
    type_: &'a Type,
    wrap_before_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match type_ {
        Type::ResolvedPath(path) => {
            format_path(context, path, false, parameter_impl_trait_cursor, output);
        }
        Type::DynTrait(dyn_trait) => {
            if wrap_before_bounds {
                output.push('(');
            }
            output.push_str("dyn ");

            let mut traits = dyn_trait
                .traits
                .iter()
                .map(|poly_trait| {
                    let mut formatted = String::new();
                    format_poly_trait(
                        context,
                        poly_trait,
                        dyn_trait.traits.len() + usize::from(dyn_trait.lifetime.is_some()) > 1,
                        parameter_impl_trait_cursor,
                        &mut formatted,
                    );
                    formatted
                })
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
                let lifetime = context.names().lifetime(lifetime);
                output.push_str(lifetime.as_ref());
            }
            if wrap_before_bounds {
                output.push(')');
            }
        }
        Type::Generic(name) => {
            let name = context.names().type_name(name);
            output.push_str(name.as_ref());
        }
        Type::Primitive(name) => output.push_str(name),
        Type::FunctionPointer(pointer) => {
            // Function-pointer ABI and safety are part of the type being
            // normalized, unlike ABI and safety on the containing function.
            let pointer_context = context.with_params(&pointer.generic_params);
            format_scoped_generic_params(&pointer_context, &pointer.generic_params, output);
            format_function_header(&pointer.header, output);
            output.push_str("fn");
            format_function_signature(
                &pointer_context,
                &pointer.sig,
                wrap_before_bounds,
                parameter_impl_trait_cursor,
                output,
            );
        }
        Type::Tuple(types) => format_tuple(context, types, parameter_impl_trait_cursor, output),
        Type::Slice(type_) => {
            output.push('[');
            format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
            output.push(']');
        }
        Type::Array { type_, len } => {
            output.push('[');
            format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
            output.push_str("; ");
            let len = context.names().const_expr(len);
            output.push_str(len.as_ref());
            output.push(']');
        }
        Type::Pat { .. } => unimplemented!("Type::Pat is unstable"),
        Type::ImplTrait(bounds) => {
            if let Some(names) = parameter_impl_trait_cursor.as_mut() {
                // We're in parameter mode, output a synthetic generic.
                output.push_str(names.next());
            } else {
                // We're in non-parameter mode, output an opaque (`impl Trait`).
                if wrap_before_bounds {
                    output.push('(');
                }
                output.push_str("impl ");
                format_bounds(context, bounds, parameter_impl_trait_cursor, output);
                if wrap_before_bounds {
                    output.push(')');
                }
            }
        }
        Type::Infer => output.push('_'),
        Type::RawPointer { is_mutable, type_ } => {
            output.push('*');
            if *is_mutable {
                output.push_str("mut");
            } else {
                output.push_str("const");
            }
            output.push(' ');
            format_type_inner(
                context,
                type_,
                wrap_before_bounds || needs_parens_before_bounds(type_),
                parameter_impl_trait_cursor,
                output,
            );
        }
        Type::BorrowedRef {
            lifetime,
            is_mutable,
            type_,
        } => {
            output.push('&');
            if let Some(lifetime) = lifetime {
                let normalized_lifetime = context.names().lifetime(lifetime);
                output.push_str(normalized_lifetime.as_ref());
                output.push(' ');
            }
            if *is_mutable {
                output.push_str("mut ");
            }
            format_type_inner(
                context,
                type_,
                wrap_before_bounds || needs_parens_before_bounds(type_),
                parameter_impl_trait_cursor,
                output,
            );
        }
        Type::QualifiedPath {
            name,
            args,
            self_type,
            trait_,
        } => {
            if let Some(trait_) = trait_ {
                if trait_.path.is_empty() {
                    format_type_inner(
                        context,
                        self_type,
                        false,
                        parameter_impl_trait_cursor,
                        output,
                    );
                } else {
                    output.push('<');
                    format_type_inner(
                        context,
                        self_type,
                        false,
                        parameter_impl_trait_cursor,
                        output,
                    );
                    output.push_str(" as ");
                    format_path(context, trait_, false, parameter_impl_trait_cursor, output);
                    output.push('>');
                }
            } else {
                format_type_inner(
                    context,
                    self_type,
                    false,
                    parameter_impl_trait_cursor,
                    output,
                );
            }

            output.push_str("::");
            output.push_str(name);
            if let Some(args) = args.as_deref() {
                format_generic_args(context, args, false, parameter_impl_trait_cursor, output);
            }
        }
    }
}

fn format_tuple<'a>(
    context: &FnNormalizationContext<'a>,
    types: &'a [Type],
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match types {
        [] => output.push_str("()"),
        [type_] => {
            output.push('(');
            format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
            output.push_str(",)");
        }
        _ => {
            output.push('(');
            for (index, type_) in types.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
            }
            output.push(')');
        }
    }
}

fn format_poly_trait<'a>(
    context: &FnNormalizationContext<'a>,
    poly_trait: &'a rustdoc_types::PolyTrait,
    wrap_before_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    let context = context.with_params(&poly_trait.generic_params);
    format_scoped_generic_params(&context, &poly_trait.generic_params, output);
    format_path(
        &context,
        &poly_trait.trait_,
        wrap_before_bounds,
        parameter_impl_trait_cursor,
        output,
    );
}

fn format_path<'a>(
    context: &FnNormalizationContext<'a>,
    path: &'a rustdoc_types::Path,
    wrap_args_before_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    output.push_str(&paths::normalized_path(context.crate_(), path));
    if let Some(args) = path.args.as_deref() {
        format_generic_args(
            context,
            args,
            wrap_args_before_bounds,
            parameter_impl_trait_cursor,
            output,
        );
    }
}

fn format_generic_args<'a>(
    context: &FnNormalizationContext<'a>,
    args: &'a rustdoc_types::GenericArgs,
    wrap_output_before_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match args {
        rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
            if args.is_empty() && constraints.is_empty() {
                return;
            }

            output.push('<');
            let mut needs_separator = false;
            for arg in args {
                if needs_separator {
                    output.push_str(", ");
                }
                format_generic_arg(context, arg, parameter_impl_trait_cursor, output);
                needs_separator = true;
            }

            let mut constraints = constraints
                .iter()
                .map(|constraint| {
                    let mut formatted = String::new();
                    format_assoc_item_constraint(
                        context,
                        constraint,
                        parameter_impl_trait_cursor,
                        &mut formatted,
                    );
                    formatted
                })
                .collect::<Vec<_>>();
            constraints.sort_unstable();
            if !constraints.is_empty() {
                if needs_separator {
                    output.push_str(", ");
                }
                let mut constraints_iter = constraints.iter();
                if let Some(constraint) = constraints_iter.next() {
                    output.push_str(constraint);
                }
                for constraint in constraints_iter {
                    output.push_str(", ");
                    output.push_str(constraint);
                }
            }
            output.push('>');
        }
        rustdoc_types::GenericArgs::Parenthesized {
            inputs,
            output: return_type,
        } => {
            output.push('(');
            for (index, type_) in inputs.iter().enumerate() {
                if index != 0 {
                    output.push_str(", ");
                }
                format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
            }
            output.push(')');

            if let Some(return_type) = return_type {
                output.push_str(" -> ");
                format_type_inner(
                    context,
                    return_type,
                    wrap_output_before_bounds,
                    parameter_impl_trait_cursor,
                    output,
                );
            }
        }
        rustdoc_types::GenericArgs::ReturnTypeNotation => output.push_str("(..)"),
    }
}

fn format_generic_arg<'a>(
    context: &FnNormalizationContext<'a>,
    arg: &'a GenericArg,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match arg {
        GenericArg::Lifetime(lifetime) => {
            let lifetime = context.names().lifetime(lifetime);
            output.push_str(lifetime.as_ref());
        }
        GenericArg::Type(type_) => {
            format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
        }
        GenericArg::Const(constant) => format_constant(context, constant, output),
        GenericArg::Infer => output.push('_'),
    }
}

fn format_assoc_item_constraint<'a>(
    context: &FnNormalizationContext<'a>,
    constraint: &'a AssocItemConstraint,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    output.push_str(&constraint.name);
    if let Some(args) = constraint.args.as_deref() {
        format_generic_args(context, args, false, parameter_impl_trait_cursor, output);
    }

    match &constraint.binding {
        AssocItemConstraintKind::Constraint(bounds) => {
            output.push_str(": ");
            format_bounds(context, bounds, parameter_impl_trait_cursor, output);
        }
        AssocItemConstraintKind::Equality(term) => {
            output.push_str(" = ");
            format_term(context, term, parameter_impl_trait_cursor, output);
        }
    }
}

fn format_bounds<'a>(
    context: &FnNormalizationContext<'a>,
    bounds: &'a [GenericBound],
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    let mut bounds = bounds
        .iter()
        .map(|bound| {
            let mut formatted = String::new();
            format_generic_bound(
                context,
                bound,
                bounds.len() > 1,
                parameter_impl_trait_cursor,
                &mut formatted,
            );
            formatted
        })
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
    context: &FnNormalizationContext<'a>,
    bound: &'a GenericBound,
    wrap_before_or_after_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match bound {
        GenericBound::TraitBound {
            trait_,
            generic_params,
            modifier,
        } => {
            let context = context.with_params(generic_params);
            format_scoped_generic_params(&context, generic_params, output);
            match modifier {
                TraitBoundModifier::None => {}
                TraitBoundModifier::Maybe => output.push('?'),
                // The schema has no separate const-trait facet. Render the stable
                // ordinary-bound view instead of leaking nightly-only `[const]` syntax.
                TraitBoundModifier::MaybeConst => {}
            }
            format_path(
                &context,
                trait_,
                wrap_before_or_after_bounds,
                parameter_impl_trait_cursor,
                output,
            );
        }
        GenericBound::Outlives(lifetime) => {
            let lifetime = context.names().lifetime(lifetime);
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
                        let lifetime = context.names().lifetime(lifetime);
                        output.push_str(lifetime.as_ref());
                    }
                    rustdoc_types::PreciseCapturingArg::Param(param) => {
                        let param = context.names().type_or_const_name(param);
                        output.push_str(param.as_ref());
                    }
                }
            }
            output.push('>');
        }
    }
}

fn format_scoped_generic_params<'a>(
    context: &FnNormalizationContext<'a>,
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
        format_generic_param_def(context, param, output);
    }
    output.push_str("> ");
}

fn format_generic_param_def<'a>(
    context: &FnNormalizationContext<'a>,
    param: &'a GenericParamDef,
    output: &mut String,
) {
    match &param.kind {
        GenericParamDefKind::Lifetime { .. } => {
            let lifetime = context.names().lifetime(&param.name);
            output.push_str(lifetime.as_ref());
        }
        GenericParamDefKind::Type { .. } => {
            // These generic params come from higher-ranked `for<...>` binders
            // on `fn` pointers or trait bounds. As of Rust 1.96, hypothetical
            // `for<T>` binders are rejected, so there is no normalized
            // signature text to produce for such a parameter.
            unreachable!("found type generic param definition in HRTB position: {param:?}");
        }
        GenericParamDefKind::Const { .. } => {
            // These generic params come from higher-ranked `for<...>` binders
            // on `fn` pointers or trait bounds. As of Rust 1.96, hypothetical
            // `for<const N: usize>` binders are rejected, so there is no
            // normalized signature text to produce for such a parameter.
            unreachable!("found const generic param definition in HRTB position: {param:?}");
        }
    }
}

fn format_function_signature<'a>(
    context: &FnNormalizationContext<'a>,
    signature: &'a rustdoc_types::FunctionSignature,
    wrap_output_before_bounds: bool,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    output.push('(');
    let mut needs_separator = false;
    for (_, type_) in &signature.inputs {
        if needs_separator {
            output.push_str(", ");
        }
        format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
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
        format_type_inner(
            context,
            return_type,
            wrap_output_before_bounds,
            parameter_impl_trait_cursor,
            output,
        );
    }
}

fn format_function_header(header: &FunctionHeader, output: &mut String) {
    if header.is_const {
        // As of Rust 1.96, function pointer types cannot use hypothetical
        // `const fn(...)` syntax, so there is no normalized signature text to
        // produce for this header shape.
        unreachable!("found const function pointer header: {header:?}");
    }
    if header.is_async {
        // As of Rust 1.96, function pointer types cannot use hypothetical
        // `async fn(...)` syntax, so there is no normalized signature text to
        // produce for this header shape.
        unreachable!("found async function pointer header: {header:?}");
    }
    if header.is_unsafe {
        output.push_str("unsafe ");
    }

    match &header.abi {
        Abi::Rust => {}
        Abi::C { unwind } => push_abi(output, "C", *unwind),
        Abi::Cdecl { unwind } => push_abi(output, "cdecl", *unwind),
        Abi::Stdcall { unwind } => push_abi(output, "stdcall", *unwind),
        Abi::Fastcall { unwind } => push_abi(output, "fastcall", *unwind),
        Abi::Aapcs { unwind } => push_abi(output, "aapcs", *unwind),
        Abi::Win64 { unwind } => push_abi(output, "win64", *unwind),
        Abi::SysV64 { unwind } => push_abi(output, "sysv64", *unwind),
        Abi::System { unwind } => push_abi(output, "system", *unwind),
        Abi::Other(other) => {
            output.push_str("extern \"");
            output.push_str(other);
            output.push_str("\" ");
        }
    }
}

fn push_abi(output: &mut String, name: &str, unwind: bool) {
    output.push_str("extern \"");
    output.push_str(name);
    if unwind {
        output.push_str("-unwind");
    }
    output.push_str("\" ");
}

fn format_constant<'a>(
    context: &FnNormalizationContext<'a>,
    constant: &'a rustdoc_types::Constant,
    output: &mut String,
) {
    let value = constant.value.as_deref().unwrap_or(&constant.expr);
    let value = context.names().const_expr(value);
    output.push_str(value.as_ref());
}

fn format_term<'a>(
    context: &FnNormalizationContext<'a>,
    term: &'a Term,
    parameter_impl_trait_cursor: &mut Option<ParameterImplTraitCursor<'_>>,
    output: &mut String,
) {
    match term {
        Term::Type(type_) => {
            format_type_inner(context, type_, false, parameter_impl_trait_cursor, output);
        }
        Term::Constant(constant) => {
            // As of Rust 1.96, associated const equality constraints such as
            // `T: Trait<N = 3>` are incomplete and rejected, so there is no
            // normalized signature text to produce for such a term.
            unreachable!("found associated const equality constraint term: {constant:?}");
        }
    }
}

fn needs_parens_before_bounds(type_: &Type) -> bool {
    match type_ {
        Type::DynTrait(dyn_trait) => {
            dyn_trait.traits.len() + usize::from(dyn_trait.lifetime.is_some()) > 1
        }
        Type::ImplTrait(bounds) => bounds.len() > 1,
        _ => false,
    }
}
