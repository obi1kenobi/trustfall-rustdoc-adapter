use rustdoc_types::{Item, Trait};

use crate::IndexedCrate;

pub(crate) fn is_trait_sealed<'a>(indexed_crate: &IndexedCrate<'a>, trait_item: &'a Item) -> bool {
    let trait_inner = unwrap_trait(trait_item);

    // If the trait is pub-in-priv, trivially sealed.
    if is_pub_in_priv_item(indexed_crate, &trait_item.id) {
        return true;
    }

    // Does the trait have a supertrait that is:
    // - defined in this crate
    // - pub-in-priv, or otherwise sealed
    if has_sealed_supertrait(indexed_crate, trait_inner) {
        return true;
    }

    // Does the trait have a method that:
    // - does not have a default impl, and
    // - takes at least one non-`self` argument that is pub-in-priv
    //
    // If so, the trait is method-sealed, per:
    // https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/#sealing-traits-via-method-signatures
    if is_method_sealed(indexed_crate, trait_inner) {
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
            rustdoc_types::GenericBound::Outlives(_) => {
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
        if is_trait_sealed(indexed_crate, supertrait) {
            // N.B.: This cannot infinite-loop, since rustc denies cyclic trait bounds.
            return true;
        }
    }

    false
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
