#[cfg(not(feature = "rustc-hash"))]
use std::collections::{HashMap, HashSet};

#[cfg(feature = "rustc-hash")]
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use rustdoc_types::{GenericBound, Id, Item, Trait};

use crate::item_flags::ItemFlag;

/// Update the `flags` with trait sealing and blanket impls information.
///
/// # Preconditions
/// - `flags` must contain complete reachability information,
///   including info on `doc(hidden)` importable paths.
pub(crate) fn compute_trait_flags(index: &HashMap<Id, Item>, flags: &mut HashMap<Id, ItemFlag>) {
    let mut possibly_sealed = Vec::with_capacity(128);
    let mut definitely_not_fully_sealed: HashSet<Id> = HashSet::default();
    for (id, item) in index.iter() {
        let trait_inner = match &item.inner {
            rustdoc_types::ItemEnum::Trait(t) => t,
            _ => continue,
        };
        let item_flags = flags.get_mut(id).expect("item flags weren't initialized");

        // First, check for blanket impls.
        for impl_id in &trait_inner.implementations {
            let Some(impl_item) = index.get(impl_id) else {
                // Probably a rustdoc bug -- the impl isn't part of the crate's index.
                continue;
            };
            let impl_inner = match &impl_item.inner {
                rustdoc_types::ItemEnum::Impl(impl_inner) => impl_inner,
                _ => {
                    unreachable!(
                        "referenced trait impl is actually not an impl item: {impl_item:?}"
                    );
                }
            };

            // N.B.: Confusingly, the `blanket_impl` field in rustdoc JSON uses
            // a different definition of "blanket" that only covers synthetic impls.
            //
            // More info:
            // https://github.com/rust-lang/rust/issues/136557#issuecomment-2634994515
            //
            // As a result, we have our own logic to determine if the impl is a blanket impl.
            if can_blanket_impl_target_include_downstream_types(impl_inner) {
                item_flags.set_trait_has_blanket_impls();
            }
        }

        if !item_flags.is_reachable() {
            // The trait isn't importable at all.
            // Either it's pub-in-priv or just not pub at all.
            // So it's trivially sealed. Nothing further to check here.
            item_flags.set_unconditionally_sealed();
            continue;
        } else if item_flags.is_non_pub_api_reachable() {
            // The trait is reachable only via `doc(hidden)` paths.
            // Downstream crates can only `impl` it by naming such a non-public-API path,
            // so the trait is public-API-sealed.
            item_flags.set_pub_api_sealed();

            // The trait might be completely sealed though, so we'll keep looking.
        }

        // Does the trait have a method that:
        // - does not have a default impl, and
        // - takes at least one non-`self` argument that is not importable
        //   (is non-pub, or is pub-in-priv)
        //
        // If so, the trait is method-sealed, per:
        // https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/#sealing-traits-via-method-signatures
        //
        // Instead, if the argument is only `doc(hidden)`-reachable,
        // then the trait is public-API-sealed. Similarly, if a non-defaulted associated item
        // is `doc(hidden)` and not deprecated, the trait is public-API-sealed.
        //
        // This method applies the flags internally, and returns `true` only if
        // the trait is unconditionally sealed, meaning that we can skip further analysis for it.
        if is_method_or_item_sealed(index, id, trait_inner, flags) {
            continue;
        }

        // The only remaining way a trait here could be sealed is if it has supertraits.
        if effective_supertraits_iter(trait_inner).next().is_some() {
            possibly_sealed.push(item);
        } else {
            definitely_not_fully_sealed.insert(*id);
        }
    }

    // At this point, we've figured out all traits that are sealed because of
    // method-sealing or because they aren't publicly importable.
    // We still need to look at supertraits.
    //
    // Supertrait sealing can lead to cycles when blanket impls are present.
    // However, the vast majority of supertrait-sealed traits don't involve a blanket impl
    // on the supertrait, so we don't want to pay the perf cost of cycle-busting in those cases.
    // We use a two-step strategy:
    // - First, evaluate traits that can be proven to be sealed by virtue of a sealed supertrait
    //   with no blanket impls. No cycles are possible here -- this is a one-step analysis.
    // - Then, evaluate traits with blanket impls on supertraits. Apply cycle-busting here.
    let mut possible_cycles = Vec::with_capacity(possibly_sealed.len());
    for trait_item in possibly_sealed {
        let trait_inner = unwrap_trait(trait_item);

        let mut proven_sealed = false;
        let mut blankets_found = false;
        let mut bound_on_undecided_trait = false;
        for bound in effective_supertraits_iter(trait_inner) {
            let supertrait_item = match bound {
                GenericBound::TraitBound { trait_, .. } => {
                    if let Some(item) = index.get(&trait_.id) {
                        item
                    } else {
                        // Not an item from this crate, so it can't cause sealing.
                        //
                        // TODO: Update this when we have cross-crate analysis,
                        //       since this can cause public-API-sealing.
                        continue;
                    }
                }
                _ => unreachable!("non-trait bound found: {bound:?}"),
            };

            let supertrait_flags = flags[&supertrait_item.id];
            if supertrait_flags.trait_has_blanket_impls() {
                blankets_found = true;
            } else if supertrait_flags.is_unconditionally_sealed() {
                // Sealed supertrait with no blanket impls! This seals our trait, and is final.
                flags
                    .get_mut(&trait_item.id)
                    .expect("no flag for trait item")
                    .set_unconditionally_sealed();
                proven_sealed = true;
                break;
            } else {
                if !definitely_not_fully_sealed.contains(&supertrait_item.id) {
                    bound_on_undecided_trait = true;
                }

                if supertrait_flags.is_only_pub_api_sealed() {
                    // Public-API-sealed supertrait with no blanket impls.
                    // This means our trait is *at least* public-API-sealed.
                    // But it might still be unconditionally sealed!
                    flags
                        .get_mut(&trait_item.id)
                        .expect("no flag for trait item")
                        .set_pub_api_sealed();
                }
            }
        }
        if !proven_sealed && (blankets_found || bound_on_undecided_trait) {
            possible_cycles.push(trait_item);
        }
    }

    // We've resolved all the easy cases. Time to deal with traits with possible cyclic bounds.
    //
    // First, check for unconditional sealing.
    let mut visited_trait_ids: HashSet<Id> = HashSet::default();
    for trait_item in &possible_cycles {
        visited_trait_ids.insert(trait_item.id);
        is_trait_supertrait_sealed_avoiding_cycles(
            index,
            trait_item,
            flags,
            &mut visited_trait_ids,
            false,
        );
        visited_trait_ids.clear();
    }

    // Then, check for public-API-sealed traits.
    for trait_item in &possible_cycles {
        visited_trait_ids.insert(trait_item.id);
        is_trait_supertrait_sealed_avoiding_cycles(
            index,
            trait_item,
            flags,
            &mut visited_trait_ids,
            true,
        );
        visited_trait_ids.clear();
    }
}

fn determine_if_trait_is_sealed_with_no_external_blankets(
    index: &HashMap<Id, Item>,
    trait_item: &Item,
    flags: &mut HashMap<Id, ItemFlag>,
    visited_trait_ids: &mut HashSet<Id>,
    consider_public_api_sealed: bool,
) -> bool {
    if !visited_trait_ids.insert(trait_item.id) {
        // Already visited this supertrait, we're in a cycle. Unwind the cycle,
        // marking all traits in it as sealed.
        if consider_public_api_sealed {
            visited_trait_ids.iter().for_each(|id| {
                flags
                    .get_mut(id)
                    .expect("no flags for trait ID")
                    .set_pub_api_sealed();
            });
        } else {
            visited_trait_ids.iter().for_each(|id| {
                flags
                    .get_mut(id)
                    .expect("no flags for trait ID")
                    .set_unconditionally_sealed();
            });
        }
        return true;
    }

    if is_trait_supertrait_sealed_avoiding_cycles(
        index,
        trait_item,
        flags,
        visited_trait_ids,
        consider_public_api_sealed,
    ) {
        let trait_flags = flags[&trait_item.id];
        let trait_inner = unwrap_trait(trait_item);
        if !trait_flags.trait_has_blanket_impls()
            || has_no_externally_satifiable_blanket_impls(
                index,
                trait_inner,
                flags,
                visited_trait_ids,
                consider_public_api_sealed,
            )
        {
            return true;
        }
    }

    visited_trait_ids.remove(&trait_item.id);

    false
}

fn is_trait_supertrait_sealed_avoiding_cycles(
    index: &HashMap<Id, Item>,
    trait_item: &Item,
    flags: &mut HashMap<Id, ItemFlag>,
    visited_trait_ids: &mut HashSet<Id>,
    consider_public_api_sealed: bool,
) -> bool {
    let trait_flag = flags[&trait_item.id];
    if trait_flag.is_unconditionally_sealed()
        || (consider_public_api_sealed && trait_flag.is_only_pub_api_sealed())
    {
        return true;
    }

    let trait_inner = unwrap_trait(trait_item);
    for bound in effective_supertraits_iter(trait_inner) {
        let supertrait_item = match bound {
            GenericBound::TraitBound { trait_, .. } => {
                if let Some(item) = index.get(&trait_.id) {
                    item
                } else {
                    // Not an item from this crate, so it can't cause sealing.
                    //
                    // TODO: Update this when we have cross-crate analysis,
                    //       since this can cause public-API-sealing.
                    continue;
                }
            }
            _ => unreachable!("non-trait bound found: {bound:?}"),
        };
        if determine_if_trait_is_sealed_with_no_external_blankets(
            index,
            supertrait_item,
            flags,
            visited_trait_ids,
            consider_public_api_sealed,
        ) {
            let trait_flags = flags
                .get_mut(&trait_item.id)
                .expect("no flags for trait ID");
            if consider_public_api_sealed {
                trait_flags.set_pub_api_sealed();
            } else {
                trait_flags.set_unconditionally_sealed();
                break;
            }
        }
    }

    let trait_flag = flags[&trait_item.id];
    trait_flag.is_unconditionally_sealed()
        || (consider_public_api_sealed && trait_flag.is_only_pub_api_sealed())
}

fn has_no_externally_satifiable_blanket_impls(
    index: &HashMap<Id, Item>,
    trait_inner: &Trait,
    flags: &mut HashMap<Id, ItemFlag>,
    visited_trait_ids: &mut HashSet<Id>,
    consider_public_api_sealed: bool,
) -> bool {
    for impl_id in &trait_inner.implementations {
        let impl_item = match index.get(impl_id).map(|item| {
            let rustdoc_types::ItemEnum::Impl(impl_item) = &item.inner else {
                panic!("impl Id {impl_id:?} did not refer to an impl item: {item:?}");
            };
            impl_item
        }) {
            Some(item) => item,
            None => {
                // Failed to find the impl item in the index.
                continue;
            }
        };

        if is_externally_satisfiable_blanket_impl(
            index,
            impl_item,
            flags,
            visited_trait_ids,
            consider_public_api_sealed,
        ) {
            return false;
        }
    }

    true
}

fn is_externally_satisfiable_blanket_impl(
    index: &HashMap<Id, Item>,
    impl_item: &rustdoc_types::Impl,
    flags: &mut HashMap<Id, ItemFlag>,
    visited_trait_ids: &mut HashSet<Id>,
    consider_public_api_sealed: bool,
) -> bool {
    // Is this a blanket impl, and can the blanket cover a type defined in a downstream crate?
    // For example, `T` and `&T` count, whereas `Vec<T>`, `[T]`, and `*const T` do not.
    if !can_blanket_impl_target_include_downstream_types(impl_item) {
        // This impl doesn't cover types of a downstream crate. It isn't relevant here.
        return false;
    }

    // Can the bounds on this impl be satisfied by downstream crates' types?
    for generic in &impl_item.generics.params {
        match &generic.kind {
            rustdoc_types::GenericParamDefKind::Type {
                bounds,
                is_synthetic,
                ..
            } => {
                if *is_synthetic {
                    // Synthetic bounds don't count. We also don't really expect to find one here.
                    continue;
                }

                // The blanket impl is only not externally satisfiable if at least one trait bound
                // references a trait where all of the following apply:
                // - The trait is local to the crate we're analyzing.
                // - The trait is sealed / public-API-sealed (depending on our bool input flag).
                // - The trait has no blanket impls that are externally satisfiable.
                //   (The same criterion we're in the middle of evaluating for another trait here.)
                for bound in bounds {
                    let rustdoc_types::GenericBound::TraitBound { trait_, .. } = bound else {
                        // Other kinds of generic bounds aren't relevant here.
                        continue;
                    };

                    let bound_trait_id = &trait_.id;
                    let Some(bound_item) = index.get(bound_trait_id) else {
                        // Not a trait from this crate.
                        //
                        // TODO: Update this when we have cross-crate analysis,
                        //       since this can cause public-API-sealing.
                        continue;
                    };

                    if determine_if_trait_is_sealed_with_no_external_blankets(
                        index,
                        bound_item,
                        flags,
                        visited_trait_ids,
                        consider_public_api_sealed,
                    ) {
                        return false;
                    }
                }
            }
            rustdoc_types::GenericParamDefKind::Lifetime { .. }
            | rustdoc_types::GenericParamDefKind::Const { .. } => {
                // Lifetime and const generics aren't relevant here.
                continue;
            }
        }
    }

    true
}

fn is_method_or_item_sealed(
    index: &HashMap<Id, Item>,
    trait_id: &Id,
    trait_inner: &Trait,
    flags: &mut HashMap<Id, ItemFlag>,
) -> bool {
    for inner_item_id in &trait_inner.items {
        let inner_item = &index.get(inner_item_id);
        let Some(inner_item) = inner_item else {
            // This is almost certainly a bug in rustdoc JSON,
            // since the trait's item isn't part of the trait's crate index.
            continue;
        };

        let assoc_item_flag = flags
            .get(inner_item_id)
            .expect("no flags entry for trait associated item");

        match &inner_item.inner {
            rustdoc_types::ItemEnum::Function(func) => {
                if func.has_body {
                    // This trait function has a default implementation.
                    // An implementation is not required in order to implement this trait on a type.
                    // Therefore, it cannot on its own cause the trait to be sealed.
                    continue;
                }

                if !assoc_item_flag.is_pub_reachable() && assoc_item_flag.is_non_pub_api_reachable()
                {
                    // This associated item is `doc(hidden)` and required to implement the trait.
                    // That makes the trait public-API-sealed.
                    flags
                        .get_mut(trait_id)
                        .expect("no flags entry for trait item ID")
                        .set_pub_api_sealed();
                }

                // Check for pub-in-priv function parameters.
                for (_, param) in &func.sig.inputs {
                    if let rustdoc_types::Type::ResolvedPath(path) = param {
                        if let Some(item_flag) = flags.get(&path.id) {
                            if !item_flag.is_reachable() {
                                // Non-importable item, so this trait is method-sealed.
                                flags
                                    .get_mut(trait_id)
                                    .expect("no flags entry for trait item ID")
                                    .set_unconditionally_sealed();
                                return true;
                            } else if item_flag.is_non_pub_api_reachable() {
                                flags
                                    .get_mut(trait_id)
                                    .expect("no flags entry for trait item ID")
                                    .set_pub_api_sealed();
                            }
                        };
                    }
                }

                // Check for pub-in-priv function return values.
                if let Some(rustdoc_types::Type::ResolvedPath(path)) = &func.sig.output {
                    if let Some(item_flag) = flags.get(&path.id) {
                        if !item_flag.is_reachable() {
                            // Non-importable item, so this trait is method-sealed.
                            flags
                                .get_mut(trait_id)
                                .expect("no flags entry for trait item ID")
                                .set_unconditionally_sealed();
                            return true;
                        } else if item_flag.is_non_pub_api_reachable() {
                            flags
                                .get_mut(trait_id)
                                .expect("no flags entry for trait item ID")
                                .set_pub_api_sealed();
                        }
                    };
                }
            }
            rustdoc_types::ItemEnum::AssocType { type_, .. } if type_.is_none() => {
                // Associated types without a default can cause a trait to be public-API-sealed.

                if !assoc_item_flag.is_pub_reachable() && assoc_item_flag.is_non_pub_api_reachable()
                {
                    // This associated item is `doc(hidden)` and required to implement the trait.
                    // That makes the trait public-API-sealed.
                    flags
                        .get_mut(trait_id)
                        .expect("no flags entry for trait item ID")
                        .set_pub_api_sealed();
                }
            }
            rustdoc_types::ItemEnum::AssocConst { type_, value } if value.is_none() => {
                // Associated constants without a default can cause a trait to be sealed,
                // either unconditionally or just public-API-sealed.

                if !assoc_item_flag.is_pub_reachable() && assoc_item_flag.is_non_pub_api_reachable()
                {
                    // This associated item is `doc(hidden)` and required to implement the trait.
                    // That makes the trait public-API-sealed.
                    flags
                        .get_mut(trait_id)
                        .expect("no flags entry for trait item ID")
                        .set_pub_api_sealed();
                }

                if let rustdoc_types::Type::ResolvedPath(path) = type_ {
                    if let Some(type_flag) = flags.get(&path.id) {
                        if !type_flag.is_reachable() {
                            // Non-importable item, so this trait is unconditionally item-sealed.
                            flags
                                .get_mut(trait_id)
                                .expect("no flags entry for trait item ID")
                                .set_unconditionally_sealed();
                            return true;
                        } else if type_flag.is_non_pub_api_reachable() {
                            flags
                                .get_mut(trait_id)
                                .expect("no flags entry for trait item ID")
                                .set_pub_api_sealed();
                        }
                    };
                }
            }
            _ => {}
        }
    }

    false
}

fn effective_supertraits_iter(trait_inner: &Trait) -> impl Iterator<Item = &GenericBound> + '_ {
    let direct_bounds = trait_inner.bounds.iter();

    let where_predicate_bounds = trait_inner.generics.where_predicates.iter().filter_map(|predicate| {
        match predicate {
            // Only `where Self: SomeTrait` predicates are relevant for sealed trait analysis.
            // Ignore all other predicate types.
            rustdoc_types::WherePredicate::BoundPredicate { type_, bounds, .. } => {
                // If the predicate isn't over `Self`, it isn't relevant.
                if matches!(type_, rustdoc_types::Type::Generic(generic) if generic.as_str() == "Self") {
                    // We found a trait similar to:
                    // `pub trait Example where Self: SealedTrait { ... }`
                    //
                    // Even though `SealedTrait` isn't *explicitly* a supertrait,
                    // any implementer of `Example` would still have to implement it too.
                    // This makes it equivalent to a supertrait bound for purposes
                    // of trait sealing.
                    Some(bounds)
                } else {
                    None
                }
            }
            _ => None,
        }
    }).flatten();

    direct_bounds
        .chain(where_predicate_bounds)
        .filter(|bound| matches!(bound, GenericBound::TraitBound { .. }))
}

fn can_blanket_impl_target_include_downstream_types(impl_item: &rustdoc_types::Impl) -> bool {
    let mut current_type = &impl_item.for_;

    loop {
        match current_type {
            rustdoc_types::Type::BorrowedRef { type_, .. } => {
                current_type = type_;
            }
            rustdoc_types::Type::ResolvedPath { .. } |  // e.g. `Arc<T>`
            rustdoc_types::Type::Tuple { .. } |         // e.g. `(T,)`
            rustdoc_types::Type::Slice { .. } |         // e.g. `[T]`
            rustdoc_types::Type::Array { .. } |         // e.g. `[T; 1]`
            rustdoc_types::Type::RawPointer { .. } |    // e.g. `*const T`
            rustdoc_types::Type::Pat { .. } => {        // unstable feature, syntax isn't finalized
                // These are all specific types that simply have a generic parameter.
                // They are not blanket implementations.
                //
                // All these types are considered "foreign" by trait coherence,
                // so Rust does not allow implementing another crate's trait on them.
                return false;
            }
            rustdoc_types::Type::Generic(..) => {
                // Blanket impl that covers downstream types!
                return true;
            }
            rustdoc_types::Type::DynTrait { .. } |        // e.g. `dyn Iterator`
            rustdoc_types::Type::Primitive { .. } |       // e.g. `i64`
            rustdoc_types::Type::FunctionPointer { .. } |
            rustdoc_types::Type::Infer |
            rustdoc_types::Type::ImplTrait { .. } |
            rustdoc_types::Type::QualifiedPath { .. } => {
                // Not a blanket impl. None of these can cover a type in a downstream crate.
                return false;
            }
        }
    }
}

fn unwrap_trait(item: &Item) -> &'_ Trait {
    match &item.inner {
        rustdoc_types::ItemEnum::Trait(t) => t,
        _ => unreachable!("item {item:?} is not a trait"),
    }
}
