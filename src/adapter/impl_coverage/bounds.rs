use rustdoc_types::{
    GenericArg, GenericBound, GenericParamDefKind, Generics, Id, ItemEnum, Path,
    TraitBoundModifier, Type, WherePredicate,
};

use crate::{
    IndexedCrate,
    adapter::positional_generic_arguments,
    hashtables::{HashMap, HashSet},
};

use super::referenced_parameter;

/// A generic argument to a trait bound or one side of an outlives bound.
///
/// `Parameter` stores an index into the owner's generic parameter list,
/// even for a bound declared on an impl.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Argument {
    Parameter(usize),
    StaticLifetime,
}

impl Argument {
    fn lifetime(name: &str, positions: &HashMap<&str, usize>) -> Option<Self> {
        match name {
            "'static" => Some(Self::StaticLifetime),
            // An impl can use a lifetime in a bound without passing it to its self type.
            // In `impl<'a: 'middle, 'b, 'middle> View<'a, 'b>`, the lookup returns `None`
            // for `'middle` because it has no owner parameter position.
            _ => positions.get(name).copied().map(Self::Parameter),
        }
    }
}

/// A trait or outlives bound whose parameter positions index the owner's
/// generic parameter list, even when the bound is declared on an impl.
/// This lets us compare the impl's bounds with the owner's when their
/// parameter names or declaration order differ.
#[derive(Debug, PartialEq, Eq, Hash)]
enum Predicate {
    Trait {
        subject_position: usize,
        trait_id: Id,
        arguments: Vec<Argument>,
    },
    /// The first argument outlives the second, as in `T: 'a` or `'a: 'b`.
    Outlives(Argument, Argument),
}

/// Checks whether the impl's bounds hold for every legal instantiation of its owner.
/// For example, if the owner is `struct Reader<T: Clone>(T)` then
/// `impl<U: Clone> Reader<U>` applies to every legal `Reader<T>` as both bounds
/// require `Clone` for the same argument.
///
/// Each owner position in the self type must receive a distinct bare impl
/// parameter or `'_`. `impl_positions` records the owner position of each
/// named parameter passed to the self type.
///
/// A `false` result means this check could not establish that the impl's bounds
/// hold for every legal instantiation.
pub(super) fn can_prove_impl_bounds(
    owner: &Generics,
    impl_generics: &Generics,
    impl_positions: &HashMap<&str, usize>,
    crate_: &IndexedCrate<'_>,
) -> bool {
    let Some(required) = predicates(impl_generics, impl_positions, crate_) else {
        return false;
    };
    // With only lifetime and const parameters, an empty predicate set leaves
    // no impl bounds to prove. A type parameter declared `T: ?Sized` can leave
    // the set empty while still requiring `MetaSized`.
    if required.is_empty()
        && !impl_generics
            .params
            .iter()
            .any(|param| matches!(param.kind, GenericParamDefKind::Type { .. }))
    {
        return true;
    }
    let Some(available) = owner_predicates(owner, crate_) else {
        return false;
    };
    required
        .difference(&available)
        .all(|missing| blanket_impl_proves_bound(missing, &available, crate_))
}

/// Checks whether a declared positive blanket impl of a nongeneric trait proves
/// the missing bound. This proof uses only declarations whose self type is a bare type
/// parameter and whose requirements already appear in the owner's normalized predicates.
///
/// For `struct CopyReader<T: Copy>(T)`, an inherent impl declared as
/// `impl<T: Copy + CopyMarker> CopyReader<T>` also requires `T: CopyMarker`.
/// The blanket impl `impl<T: Copy> CopyMarker for T` supplies that bound for
/// every legal type argument, so the inherent impl covers every `CopyReader<T>`.
fn blanket_impl_proves_bound(
    missing: &Predicate,
    available: &HashSet<Predicate>,
    crate_: &IndexedCrate<'_>,
) -> bool {
    let Predicate::Trait {
        subject_position,
        trait_id,
        ..
    } = missing
    else {
        return false;
    };
    let index = &crate_.inner.index;
    let Some(item) = index.get(trait_id) else {
        return false;
    };
    let trait_ = match &item.inner {
        ItemEnum::Trait(trait_) => trait_,
        // For `trait Marked = Marker;` (an unstable feature),
        // rustdoc lists blanket impls on `Marker`. This check does not expand aliases,
        // so it cannot prove `Marked` from those impls.
        ItemEnum::TraitAlias(_) => return false,
        _ => unreachable!("trait bound refers to an item other than a trait or trait alias"),
    };
    // For `trait MarkerFor<V = u8>`, `T: MarkerFor` requires `T: MarkerFor<u8>`.
    // This proof ignores trait arguments, so accepting `impl<T> MarkerFor<u16> for T {}`
    // would incorrectly establish that bound.
    if !trait_.generics.params.is_empty() {
        return false;
    }

    trait_.implementations.iter().any(|id| {
        let ItemEnum::Impl(impl_) = &index[id].inner else {
            unreachable!("trait implementation {id:?} was not an impl");
        };

        // Only select positive blanket impls.
        let Type::Generic(name) = &impl_.for_ else {
            return false;
        };
        if impl_.is_negative {
            return false;
        }

        // The blanket impl's bare self parameter stands for the entire subject of the
        // missing bound. Give it that subject's owner position so we can compare
        // the blanket impl's requirements with the owner's predicates.
        let positions = [(name.as_str(), *subject_position)].into_iter().collect();
        predicates(&impl_.generics, &positions, crate_)
            .is_some_and(|requirements| requirements.is_subset(available))
    })
}

/// Returns one entry per owner parameter. A `true` entry means that, starting from
/// any legal instantiation, we can replace that argument with a different legal argument
/// while keeping every other argument fixed. A `false` entry means we cannot prove this.
///
/// A type parameter marked `true` appears in no owner trait bound other than `Sized`.
/// In `B: Trait<A>`, both entries are `false` because changing either type argument
/// could break the bound.
///
/// With the other arguments fixed, any `Sized` type that outlives every lifetime,
/// such as `u8` or `u16`, is a legal replacement for a type parameter marked `true`.
/// Outliving every lifetime satisfies both explicit outlives bounds and those implied
/// by the owner's fields.
///
/// Returns `None` if we cannot analyze a const parameter's type or an owner bound.
/// For example, the unsupported bound `B: Trait<Vec<A>>` can constrain both `A`
/// and `B`. Skipping it could make either argument appear free to vary.
pub(super) fn independently_variable_parameters(
    owner: &Generics,
    crate_: &IndexedCrate<'_>,
) -> Option<Vec<bool>> {
    // Integer, `bool`, and `char` const parameters have at least two possible values.
    // Require these types for every const parameter so that varying one argument
    // cannot change the type of another const parameter.
    let const_types_are_supported = owner.params.iter().all(|param| match &param.kind {
        GenericParamDefKind::Const { type_, .. } => match type_ {
            Type::Primitive(name) => [
                "bool", "char", "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32",
                "i64", "i128", "isize",
            ]
            .contains(&name.as_str()),
            _ => false,
        },
        _ => true,
    });
    if !const_types_are_supported {
        return None;
    }
    let predicates = owner_predicates(owner, crate_)?;
    let mut independent: Vec<_> = owner
        .params
        .iter()
        .map(|param| !matches!(param.kind, GenericParamDefKind::Lifetime { .. }))
        .collect();
    for predicate in predicates {
        if let Predicate::Trait {
            subject_position,
            trait_id,
            arguments,
        } = predicate
            && trait_id != crate_.sized_trait
        {
            independent[subject_position] = false;
            for arg in arguments {
                if let Argument::Parameter(position) = arg {
                    independent[position] = false;
                }
            }
        }
    }
    Some(independent)
}

fn owner_predicates(owner: &Generics, crate_: &IndexedCrate<'_>) -> Option<HashSet<Predicate>> {
    let positions = owner
        .params
        .iter()
        .enumerate()
        .map(|(position, param)| (param.name.as_str(), position))
        .collect();
    predicates(owner, &positions, crate_)
}

/// Expresses bounds in terms of the owner's parameter positions so declarations
/// can be compared even when their parameter names or declaration order differ.
/// Inline bounds and `where` clauses contribute to the same predicate set.
///
/// Returns `None` if a bound is unsupported, a parameter needed to encode a bound
/// has no owner position, or rustdoc omits a trait's defining path when the analysis
/// needs it. A partial set could omit a restriction on the declaration's arguments.
fn predicates(
    generics: &Generics,
    positions: &HashMap<&str, usize>,
    crate_: &IndexedCrate<'_>,
) -> Option<HashSet<Predicate>> {
    enum Bounds<'a> {
        Type(&'a str, &'a [GenericBound]),
        Lifetime(&'a str, &'a [String]),
        UnsupportedWherePredicate,
    }

    let inline = generics
        .params
        .iter()
        .filter_map(|param| match &param.kind {
            GenericParamDefKind::Type { bounds, .. } => Some(Bounds::Type(&param.name, bounds)),
            GenericParamDefKind::Lifetime { outlives } => {
                Some(Bounds::Lifetime(&param.name, outlives))
            }
            GenericParamDefKind::Const { .. } => None,
        });
    let where_clauses = generics
        .where_predicates
        .iter()
        .map(|predicate| match predicate {
            WherePredicate::BoundPredicate {
                type_: Type::Generic(name),
                bounds,
                generic_params,
            } if generic_params.is_empty() => Bounds::Type(name, bounds),
            WherePredicate::LifetimePredicate { lifetime, outlives } => {
                Bounds::Lifetime(lifetime, outlives)
            }
            // We record trait and outlives bounds on individual parameters. For example,
            // a bound on `Vec<T>` has a compound subject, and a predicate with `for<'a>`
            // introduces a lifetime that has no owner position.
            _ => Bounds::UnsupportedWherePredicate,
        });

    let mut facts = HashSet::default();
    let mut relaxed_sized = HashSet::<usize>::default();
    for bounds in inline.chain(where_clauses) {
        match bounds {
            Bounds::UnsupportedWherePredicate => {
                // We found an unsupported `where` predicate, so bail out.
                return None;
            }
            Bounds::Lifetime(name, outlives) => {
                for target in outlives {
                    // A lifetime outlives itself, and `'static` outlives every lifetime.
                    // Both facts hold without an owner position.
                    if name == "'static" || name == target {
                        continue;
                    }
                    // On an impl of `View<'a, 'b>`, `'a: 'middle` and `'middle: 'b`
                    // cannot be represented because `'middle` has no owner position.
                    // Return `None` because the remaining bounds would omit
                    // the requirement that `'a` outlive `'b`.
                    facts.insert(Predicate::Outlives(
                        Argument::lifetime(name, positions)?,
                        Argument::lifetime(target, positions)?,
                    ));
                }
            }
            Bounds::Type(name, bounds) => {
                let Some(&subject) = positions.get(name) else {
                    // In `impl<T, U: Clone> Reader<T> where T: Iterator<Item = U>`,
                    // `U` has no owner position. `U: Clone` constrains `T::Item`,
                    // which we cannot express as a bound on an owner parameter.
                    return None;
                };
                for bound in bounds {
                    match bound {
                        GenericBound::Outlives(lifetime) => {
                            facts.insert(Predicate::Outlives(
                                Argument::Parameter(subject),
                                Argument::lifetime(lifetime, positions)?,
                            ));
                        }
                        GenericBound::TraitBound {
                            trait_,
                            generic_params,
                            modifier,
                        } if generic_params.is_empty() => match modifier {
                            TraitBoundModifier::Maybe if trait_.id == crate_.sized_trait => {
                                relaxed_sized.insert(subject);
                            }
                            TraitBoundModifier::None => {
                                facts.insert(trait_predicate(
                                    subject, trait_, generics, positions, crate_,
                                )?);
                            }
                            _ => {
                                // Apart from `?Sized`, we only compare ordinary trait bounds.
                                // Ignoring a modifier could make different requirements
                                // compare equal, so we bail instead.
                                return None;
                            }
                        },
                        // For example, the lifetime in `T: for<'a> Trait<'a>` belongs to the bound,
                        // so we cannot express that trait argument as an owner parameter position.
                        _ => return None,
                    }
                }
            }
        }
    }

    // Insert implicit `Sized` bounds after collecting relaxations from inline bounds
    // and `where` clauses. `?Sized` suppresses the implicit bound and leaves any
    // explicit `Sized` requirement in `facts`.
    for param in &generics.params {
        if matches!(param.kind, GenericParamDefKind::Type { .. }) {
            let position = positions[param.name.as_str()];
            if !relaxed_sized.contains(&position) {
                facts.insert(Predicate::Trait {
                    subject_position: position,
                    trait_id: crate_.sized_trait,
                    arguments: Vec::new(),
                });
            }
        }
    }
    Some(facts)
}

/// Encodes an ordinary trait bound using the owner's parameter positions.
/// Returns `None` when the bound cannot be represented or when it may change
/// which implicit sizing bound applies.
fn trait_predicate(
    subject: usize,
    trait_: &Path,
    generics: &Generics,
    positions: &HashMap<&str, usize>,
    crate_: &IndexedCrate<'_>,
) -> Option<Predicate> {
    // Use the defining path to distinguish the built-in sizing traits
    // from local traits of the same name.
    let Some(summary) = crate_.inner.paths.get(&trait_.id) else {
        // Rustdoc JSON bugs can leave a referenced ID without a path entry.
        // See https://github.com/rust-lang/rust/issues/113674 for one example.
        // Without the defining path, we cannot tell whether the bound replaces implicit `Sized`.
        return None;
    };
    if matches!(summary.path.as_slice(), [crate_name, module, name]
        if crate_name == "core" && module == "marker"
            && matches!(name.as_str(), "MetaSized" | "PointeeSized"))
    {
        // Both bounds replace implicit `Sized`, which ordinary bounds leave in place.
        // Encoding both as `?Sized` would lose a distinction. `?Sized` requires
        // `MetaSized`, which excludes extern types that `PointeeSized` allows.
        return None;
    }

    let Some(args) = positional_generic_arguments(trait_) else {
        // Trait bounds can carry information beyond their positional arguments.
        // Dropping an associated item constraint, for example, would make
        // `Iterator<Item = u8>` compare equal to `Iterator`.
        return None;
    };

    // Compare only bare parameters and `'static` as trait arguments.
    let arguments = args
        .iter()
        .map(|arg| match arg {
            GenericArg::Lifetime(name) => Argument::lifetime(name, positions),
            _ => {
                // Rustdoc can print distinct const expressions as the same `{ _ }`.
                // Only accept a const argument when it names a parameter.
                let param = referenced_parameter(arg, generics)?;
                positions
                    .get(param.name.as_str())
                    .copied()
                    .map(Argument::Parameter)
            }
        })
        .collect::<Option<Vec<_>>>()?;
    Some(Predicate::Trait {
        subject_position: subject,
        trait_id: trait_.id,
        arguments,
    })
}
