use rustdoc_types::{
    GenericArg, GenericParamDef, GenericParamDefKind, Generics, Impl, ItemEnum, ItemKind, Type,
};

use crate::{IndexedCrate, hashtables::HashMap};

use super::positional_generic_arguments;

mod bounds;

pub(super) enum ImplCoverage {
    All,
    NotAll,
    Unknown,
}

/// Checks whether an inherent impl covers every instantiation permitted by its owner.
///
/// The owner is the underlying type declaration for the impl's self type. For example,
/// `impl Foo` and `impl Trait for Foo` both have owner `Foo`.
///
/// For a struct `Foo`, `Struct.impls` can also contain the ID of an `Impl` record for
/// `impl<T> MyTrait for T` as applied to `Foo`. That record has `Foo` in `Impl.for_`
/// and `T` in `Impl.blanket_impl`. The blanket declaration's self type is `T`,
/// so the declaration has no named owner. The analysis currently returns
/// [`ImplCoverage::Unknown`] for all trait impls, including these records.
pub(super) fn impl_coverage(impl_: &Impl, crate_: &IndexedCrate<'_>) -> ImplCoverage {
    if impl_.trait_.is_some() {
        // TODO: When checking direct positive trait impls, account for bounds implied
        // by the trait arguments. For example, using `impl<T> Decode<&'static T> for Reader<T>`
        // requires `T: 'static` even when the impl has no explicit lifetime bound.
        return ImplCoverage::Unknown;
    }
    let Type::ResolvedPath(path) = &impl_.for_ else {
        // To check coverage we need a path to the owner's declaration. Inherent impls
        // on built-in types such as `u8`, `[T]`, and `*const T` have no such path.
        return ImplCoverage::Unknown;
    };
    let Some(owner) = crate_.inner.index.get(&path.id) else {
        // Without the owner's declaration, we can't check which arguments its bounds permit.
        return ImplCoverage::Unknown;
    };
    let owner_generics = match &owner.inner {
        ItemEnum::Struct(s) => &s.generics,
        ItemEnum::Enum(e) => &e.generics,
        ItemEnum::Union(u) => &u.generics,
        _ => unreachable!(
            "inherent impl self type `{}` must resolve to a struct, enum, or union",
            path.path
        ),
    };
    if owner_generics.params.is_empty() {
        // The analysis assumes every impl it checks applies to at least one legal owner
        // instantiation (see `rustdoc_schema.graphql`). A nongeneric owner has only one
        // instantiation, so the impl must apply to it even if the impl has a `where` clause.
        return ImplCoverage::All;
    }
    let args = positional_generic_arguments(path)
        .expect("a struct, enum, or union self type can have only positional generic arguments");

    let Some(impl_positions) = forwarded_parameters(args, owner_generics, &impl_.generics) else {
        return coverage_from_self_type(args, owner_generics, &impl_.generics, crate_);
    };

    if bounds::can_prove_impl_bounds(owner_generics, &impl_.generics, &impl_positions, crate_) {
        ImplCoverage::All
    } else {
        // A bound we can't prove may still hold for every instantiation. For example,
        // we don't infer an impl's explicit `T: Clone` bound from the owner's `T: Copy`.
        ImplCoverage::Unknown
    }
}

/// Checks whether the impl's self type rules out any legal instantiation of its owner.
///
/// Returns [`ImplCoverage::NotAll`] if it can show that varying one type or const
/// argument of a covered instantiation can produce a legal instantiation excluded
/// by the impl's self type. If it establishes no exclusion, returns [`ImplCoverage::Unknown`].
fn coverage_from_self_type(
    args: &[GenericArg],
    owner: &Generics,
    impl_generics: &Generics,
    crate_: &IndexedCrate<'_>,
) -> ImplCoverage {
    let Some(independent) = bounds::independently_variable_parameters(owner, crate_) else {
        return ImplCoverage::Unknown;
    };

    let mut seen_parameters = HashMap::<&str, bool>::default();
    for (pos, can_vary) in independent.into_iter().enumerate() {
        let excludes_argument = match args.get(pos) {
            // A missing argument uses its default. Once the preceding arguments are fixed,
            // the default selects one value at this position.
            None => can_vary,
            // When `can_vary` is true for a type position, that position depends on
            // no trait bound (possibly other than `Sized`). Any `Sized + 'static` type
            // is legal here, so we could slot in e.g. `u8` and `u16` with other arguments fixed.
            // This means fixing that position to any single concrete type excludes
            // at least one of `u8` and `u16` (possibly both), finding an uncovered instantiation.
            Some(GenericArg::Type(
                Type::Primitive(_)
                | Type::Tuple(_)
                | Type::BorrowedRef { .. }
                | Type::RawPointer { .. }
                | Type::Array { .. }
                | Type::Slice(_)
                | Type::FunctionPointer(_)
                | Type::DynTrait(_),
            )) => can_vary,
            Some(GenericArg::Type(Type::ResolvedPath(path))) => {
                // Treating an unexpanded alias as a fixed type could falsely report an exclusion.
                // For example, with `type Id<T> = T`, the `Id<T>` in `Reader<Id<T>>` is just `T`.
                // Require a path to a struct, enum, or union before using this argument
                // to establish an exclusion.
                can_vary
                    && matches!(
                        crate_.inner.paths.get(&path.id).map(|item| item.kind),
                        Some(ItemKind::Struct | ItemKind::Enum | ItemKind::Union)
                    )
            }
            Some(GenericArg::Const(c)) if c.is_literal => can_vary,
            Some(arg @ (GenericArg::Type(Type::Generic(_)) | GenericArg::Const(_))) => {
                // Reusing a parameter requires both arguments to agree. Either owner
                // position can supply the counterexample if it can vary independently.
                let Some(param) = referenced_parameter(arg, impl_generics) else {
                    continue;
                };
                seen_parameters
                    .insert(param.name.as_str(), can_vary)
                    .is_some_and(|earlier_can_vary| earlier_can_vary || can_vary)
            }
            // We cannot establish an exclusion from the remaining argument forms.
            // For example, fields can require two lifetimes to outlive each other even when
            // rustdoc shows no such bound. Repeating a lifetime need not exclude any legal pair.
            _ => false,
        };
        if excludes_argument {
            return ImplCoverage::NotAll;
        }
    }

    ImplCoverage::Unknown
}

/// Maps bare named parameters in the self type to their unique owner positions.
/// This lets the bounds comparison identify parameters across renamings and reorderings.
/// Each `'_` supplies its own anonymous lifetime in the self type, so it needs no map entry.
///
/// In stable Rust, the compiler checks the kind of each argument and the type of each
/// forwarded const parameter. Rely on that check so equivalent const types can match
/// even when one declaration spells the type through an alias.
///
/// Returns `None` if an argument is omitted, if a written argument other than `'_`
/// does not resolve to a bare impl parameter, or if multiple arguments use the
/// same impl parameter. Without this mapping, bounds cannot be compared by owner position.
fn forwarded_parameters<'a>(
    args: &[GenericArg],
    owner: &Generics,
    impl_generics: &'a Generics,
) -> Option<HashMap<&'a str, usize>> {
    if args.len() < owner.params.len() {
        // For example, in `Pair<A, B = u8>`, omitting the second argument fixes it to `u8`.
        // Comparing only the written arguments would make `impl<T> Pair<T>` appear
        // to cover every `Pair<T, U>`.
        return None;
    }
    let mut positions = HashMap::default();
    for (pos, arg) in args.iter().enumerate() {
        if matches!(arg, GenericArg::Lifetime(lt) if lt == "'_") {
            continue;
        }
        let impl_param = referenced_parameter(arg, impl_generics)?;
        if positions.insert(impl_param.name.as_str(), pos).is_some() {
            // Reusing a parameter ties two owner positions together. For example,
            // `Pair<T, T>` requires equal arguments, so an instantiation with
            // two different arguments would not be covered.
            return None;
        }
    }
    Some(positions)
}

/// Looks up the parameter named by a bare argument, returning `None` when
/// no matching parameter can be identified.
///
/// For const arguments, match only nonliteral expressions. Rustdoc records
/// the parameter `r#true` as `true`, which is also the text of a literal.
/// Require a const parameter because a module constant can share its name
/// with a type parameter.
///
/// For type arguments, match only direct generics like `T`. Referencing
/// a generic type by wrapping it in another type (such as `Vec<T>`) produces `None`.
fn referenced_parameter<'a>(
    arg: &GenericArg,
    generics: &'a Generics,
) -> Option<&'a GenericParamDef> {
    match arg {
        GenericArg::Lifetime(name) | GenericArg::Type(Type::Generic(name)) => {
            generics.params.iter().find(|p| p.name == *name)
        }
        GenericArg::Const(value) if !value.is_literal => generics
            .params
            .iter()
            .find(|p| matches!(p.kind, GenericParamDefKind::Const { .. }) && p.name == value.expr),
        _ => None,
    }
}
