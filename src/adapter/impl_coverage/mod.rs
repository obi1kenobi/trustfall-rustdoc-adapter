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

/// Checks whether an impl covers every instantiation permitted by its owner.
/// The analysis handles inherent impls and declared positive trait impls whose
/// self type names a struct, enum, or union. For a trait impl on a generic owner,
/// it returns [`ImplCoverage::Unknown`] unless the trait is known to have no
/// generic parameters.
///
/// The owner is the underlying type declaration for the impl's self type. For example,
/// `impl Foo` and `impl Trait for Foo` both have owner `Foo`.
///
/// For a struct `Foo`, `Struct.impls` can also contain the ID of an `Impl` record for
/// `impl<T> MyTrait for T` as applied to `Foo`. That record has `Foo` in `Impl.for_`
/// and `T` in `Impl.blanket_impl`. The blanket declaration's self type is `T`,
/// so the declaration has no named owner. The analysis returns
/// [`ImplCoverage::Unknown`] for such records.
pub(super) fn impl_coverage(impl_: &Impl, crate_: &IndexedCrate<'_>) -> ImplCoverage {
    if impl_.is_negative || impl_.is_synthetic || impl_.blanket_impl.is_some() {
        // Coverage here describes declared positive impls. A negative impl rules out trait
        // membership, and an inferred auto-trait impl has no declaration to analyze.
        // For an application of a blanket impl, `for_` can name the type rustdoc applied
        // it to instead of the self type in the declaration.
        return ImplCoverage::Unknown;
    }
    let Type::ResolvedPath(path) = &impl_.for_ else {
        // To check coverage we need a path to the owner's declaration.
        //
        // Built-in self types such as `u8`, `[T]`, and `*const T` have no owner declaration.
        // Rustdoc also lists the `IntoIterator` impl for shared references to a `Vec`
        // among the impls for `Vec`. Using the referent as the owner would incorrectly attribute
        // an impl that iterates by reference to owned vectors.
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
        ItemEnum::TypeAlias(_) | ItemEnum::ExternType if impl_.trait_.is_some() => {
            // An alias can fix generic arguments of its underlying type. For example,
            // with `type Bytes = Vec<u8>;`, an impl for `Bytes` leaves `Vec<u16>` uncovered.
            // Without expanding the alias in the impl's self type, we cannot determine
            // whether the impl covers every legal instantiation of its owner.
            //
            // An extern type has no struct, enum, or union definition to analyze.
            //
            // The analysis leaves coverage unknown in both cases.
            return ImplCoverage::Unknown;
        }
        _ => unreachable!(
            "impl self type `{}` resolved to an unexpected item kind: {impl_:?}",
            path.path
        ),
    };
    if owner_generics.params.is_empty() {
        // The analysis assumes every impl it checks applies to at least one legal owner
        // instantiation (see `rustdoc_schema.graphql`). A nongeneric owner has only one
        // instantiation, so the impl must cover it regardless of its `where` clause
        // or the trait's declaration.
        return ImplCoverage::All;
    }
    if let Some(trait_path) = &impl_.trait_
        && !crate_.is_known_nongeneric_trait(&trait_path.id)
    {
        // A trait argument can impose a bound absent from the impl's explicit bounds.
        // Rustdoc can omit defaulted arguments from the trait path, so we need the declaration
        // or built-in trait metadata to know whether the trait has any parameters.
        //
        //     struct Label<'a>(&'a str);
        //     trait Mark<A = &'static Self> {}
        //     impl<'a> Mark for Label<'a> {}
        //
        // The self type and explicit impl bounds would give `All`, but the default
        // `&'static Label<'a>` limits this impl to `Label<'static>`, excluding labels
        // that borrow a local string.
        //
        // TODO: Account for bounds implied by trait arguments, including defaults.
        return ImplCoverage::Unknown;
    }
    let args = positional_generic_arguments(path)
        .expect("a struct, enum, or union self type can have only positional generic arguments");

    let Some(impl_positions) = forwarded_parameters(args, owner_generics, &impl_.generics) else {
        return coverage_from_self_type(args, owner_generics, &impl_.generics, crate_);
    };

    bounds::coverage_from_bounds(owner_generics, &impl_.generics, &impl_positions, crate_)
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
