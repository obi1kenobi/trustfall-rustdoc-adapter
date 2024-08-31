use rustdoc_types::{Item, Trait};

use crate::IndexedCrate;

pub(crate) fn is_trait_sealed<'a>(indexed_crate: &IndexedCrate<'a>, item: &'a Item) -> bool {
    let trait_item = unwrap_trait(item);

    // If the trait is pub-in-priv, trivially sealed.
    if is_pub_in_priv_item(indexed_crate, &item.id) {
        return true;
    }

    // Does the trait have a method that:
    // - does not have a default impl, and
    // - takes at least one non-`self` argument that is pub-in-priv
    //
    // If so, the trait is method-sealed, per:
    // https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/#sealing-traits-via-method-signatures
    if is_method_sealed(indexed_crate, trait_item) {
        return true;
    }

    // Does the trait have a supertrait that is all of the below:
    // - defined in this crate
    // - pub-in-priv, or otherwise sealed
    // - lacking a blanket impl whose bounds can be satisfied outside this crate
    if has_sealed_supertrait(indexed_crate, trait_item) {
        return true;
    }

    false
}

fn is_pub_in_priv_item<'a>(indexed_crate: &IndexedCrate<'a>, id: &'a rustdoc_types::Id) -> bool {
    // TODO: We don't need all names here, one is plenty. See if this is worth optimizing.
    indexed_crate.publicly_importable_names(id).is_empty()
}

fn unwrap_trait(item: &Item) -> &'_ Trait {
    match &item.inner {
        rustdoc_types::ItemEnum::Trait(t) => t,
        _ => unreachable!(),
    }
}

fn is_method_sealed<'a>(indexed_crate: &IndexedCrate<'a>, trait_inner: &'a Trait) -> bool {
    for inner_item_id in &trait_inner.items {
        let inner_item = &indexed_crate.inner.index[inner_item_id];
        if let rustdoc_types::ItemEnum::Function(func) = &inner_item.inner {
            if func.has_body {
                // This trait function has a default implementation.
                // An implementation is not required in order to implement this trait on a type.
                // Therefore, it cannot on its own cause the trait to be sealed.
                continue;
            }

            // Check for pub-in-priv function parameters.
            for (_, param) in &func.decl.inputs {
                if let rustdoc_types::Type::ResolvedPath(path) = param {
                    if is_local_pub_in_priv_path(indexed_crate, path) {
                        return true;
                    }
                }
            }
        }
    }

    false
}

fn has_sealed_supertrait<'a>(indexed_crate: &IndexedCrate<'a>, inner: &'a Trait) -> bool {
    for bound in &inner.bounds {
        let supertrait = match bound {
            rustdoc_types::GenericBound::TraitBound {
                trait_: trait_path, ..
            } => {
                match indexed_crate.inner.index.get(&trait_path.id) {
                    Some(item) => item,
                    None => {
                        // Item from another crate, so clearly not pub-in-priv.
                        //
                        // TODO: Once we have the ability to do cross-crate analysis, consider
                        //       whether this external trait is sealed. That can have
                        //       some interesting SemVer implications as well.
                        continue;
                    }
                }
            }
            rustdoc_types::GenericBound::Outlives(_) | rustdoc_types::GenericBound::Use(_) => {
                continue;
            }
        };

        // Otherwise, check if the supertrait is itself sealed.
        // This catches cases like:
        // ```rust
        // mod priv {
        //     pub trait Sealed {}
        // }
        //
        // pub trait First: Sealed {}
        //
        // pub trait Second: First {}
        // ```
        //
        // Here, both `First` and `Second` are sealed.
        //
        // N.B.: This cannot infinite-loop, since rustc denies cyclic trait bounds.
        if is_trait_sealed(indexed_crate, supertrait) {
            // The supertrait is sealed, so a downstream crate cannot add a direct impl for it
            // on any of its own types.
            //
            // However, this isn't the same thing as "downstream types cannot impl this trait"!
            // We must check the supertrait for blanket impls that might result in
            // a downstream type gaining an impl for that trait.
            if has_no_externally_satisfiable_blanket_impls(indexed_crate, supertrait) {
                return true;
            }
        }
    }

    false
}

fn has_no_externally_satisfiable_blanket_impls(
    indexed_crate: &IndexedCrate<'_>,
    trait_: &Item,
) -> bool {
    let trait_item = unwrap_trait(trait_);
    for impl_id in &trait_item.implementations {
        let impl_item = match indexed_crate.inner.index.get(impl_id).map(|item| {
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

        if is_externally_satisfiable_blanket_impl(indexed_crate, impl_item) {
            return false;
        }
    }

    true
}

fn is_externally_satisfiable_blanket_impl(
    indexed_crate: &IndexedCrate<'_>,
    impl_item: &rustdoc_types::Impl,
) -> bool {
    let blanket_type = match get_impl_target_if_blanket_impl(impl_item) {
        None => {
            // Not a blanket impl, so not relevant here.
            return false;
        }
        Some(blanket) => blanket,
    };

    // Can the blanket cover a type that a downstream crate might define?
    // For example, `T` and `&T` count, whereas `Vec<T>`, `[T]`, and `*const T` do not.
    if !blanket_type_might_cover_types_in_downstream_crate(blanket_type) {
        // This blanket impl doesn't cover types of a downstream crate. It isn't relevant here.
        return false;
    }

    // Can the bounds on this impl be satisfied by downstream crates' types?
    for generic in &impl_item.generics.params {
        match &generic.kind {
            rustdoc_types::GenericParamDefKind::Type {
                bounds, synthetic, ..
            } => {
                if *synthetic {
                    // Synthetic bounds don't count. We also don't really expect to find one here.
                    continue;
                }

                // The blanket impl is only not externally satisfiable if at least one trait bound
                // references a trait where all of the following apply:
                // - The trait is local to the crate we're analyzing.
                // - The trait is sealed.
                // - The trait has no blanket impls that are externally satisfiable.
                //   (The same criterion we're in the middle of evaluating for another trait here.)
                for bound in bounds {
                    if is_bounded_on_local_sealed_trait_without_blankets(indexed_crate, bound) {
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

fn get_impl_target_if_blanket_impl(
    impl_item: &rustdoc_types::Impl,
) -> Option<&rustdoc_types::Type> {
    let mut current_type = &impl_item.for_;

    loop {
        match current_type {
            rustdoc_types::Type::ResolvedPath { .. } |  // e.g. `Arc<T>`
            rustdoc_types::Type::DynTrait { .. } |      // e.g. `dyn Iterator`
            rustdoc_types::Type::Tuple { .. } |         // e.g. `(T,)`
            rustdoc_types::Type::Slice { .. } |         // e.g. `[T]`
            rustdoc_types::Type::Array { .. } |         // e.g. `[T; 1]`
            rustdoc_types::Type::Pat { .. } => {        // unstable feature, syntax isn't finalized
                // These are all specific types that simply have a generic parameter.
                // They are not blanket implementations.
                return None;
            }
            rustdoc_types::Type::Generic(..) => {
                // Blanket impl!
                break;
            }
            rustdoc_types::Type::Primitive { .. } |
            rustdoc_types::Type::FunctionPointer { .. } |
            rustdoc_types::Type::Infer |
            rustdoc_types::Type::ImplTrait { .. } |
            rustdoc_types::Type::QualifiedPath { .. } => {
                // Not a blanket impl.
                return None;
            }
            rustdoc_types::Type::RawPointer { type_, .. } |
            rustdoc_types::Type::BorrowedRef { type_, .. } => {
                current_type = type_;
            }
        }
    }

    Some(&impl_item.for_)
}

fn is_bounded_on_local_sealed_trait_without_blankets(
    indexed_crate: &IndexedCrate<'_>,
    bound: &rustdoc_types::GenericBound,
) -> bool {
    match bound {
        rustdoc_types::GenericBound::TraitBound { trait_, .. } => {
            let bound_trait_id = &trait_.id;
            let Some(item) = indexed_crate.inner.index.get(bound_trait_id) else {
                // Not a trait from this crate.
                return false;
            };

            // We cannot have an infinite loop here, since Rust won't allow cyclic bounds.
            if !is_trait_sealed(indexed_crate, item) {
                return false;
            }

            has_no_externally_satisfiable_blanket_impls(indexed_crate, item)
        }
        rustdoc_types::GenericBound::Outlives(_) | rustdoc_types::GenericBound::Use(_) => {
            // Other kinds of generic bounds aren't relevant here.
            false
        }
    }
}

fn blanket_type_might_cover_types_in_downstream_crate(blanket_type: &rustdoc_types::Type) -> bool {
    match blanket_type {
        rustdoc_types::Type::Generic(..) => {
            // Blanket implementation over a bare generic type, like `T`.
            // This matches!
            true
        }
        rustdoc_types::Type::BorrowedRef { type_, .. } => {
            // Blanket implementatio over a reference, like `&T`.
            // It matches if the underlying type beheath the reference matches.
            blanket_type_might_cover_types_in_downstream_crate(type_)
        }
        rustdoc_types::Type::ResolvedPath { .. } |  // e.g. `Arc<T>`
        rustdoc_types::Type::Tuple { .. } |         // e.g. `(T,)`
        rustdoc_types::Type::Slice { .. } |         // e.g. `[T]`
        rustdoc_types::Type::Array { .. } |         // e.g. `[T; 1]`
        rustdoc_types::Type::RawPointer { .. } => { // e.g. `*const T`
            // All these types are considered "foreign" by trait coherence,
            // so Rust does not allow implementing another crate's trait on them.
            false
        }
        rustdoc_types::Type::DynTrait { .. } |
        rustdoc_types::Type::Primitive { .. } |
        rustdoc_types::Type::FunctionPointer { .. } |
        rustdoc_types::Type::Pat { .. } |
        rustdoc_types::Type::ImplTrait { .. } |
        rustdoc_types::Type::Infer { .. } |
        rustdoc_types::Type::QualifiedPath { .. } => {
            // None of these can cover a type in a downstream crate.
            false
        }
    }
}

fn is_local_pub_in_priv_path<'a>(
    indexed_crate: &IndexedCrate<'a>,
    path: &'a rustdoc_types::Path,
) -> bool {
    let Some(item) = indexed_crate.inner.index.get(&path.id) else {
        // Not an item in this crate.
        return false;
    };
    is_pub_in_priv_item(indexed_crate, &item.id)
}
