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
    // - does not have a default impl, and either:
    //   a. takes at least one non-`self` argument that is pub-in-priv, or
    //   b. has a generic parameter that causes it to be generic-sealed
    // If so, the trait is method-sealed or generic-sealed, per:
    // https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/#sealing-traits-via-method-signatures
    // https://jack.wrenn.fyi/blog/private-trait-methods/
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=3393c3ae143cb75c9da7bfe6e8ff8084
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
                        return false;
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

            // Check for generics-sealed methods, as described in:
            // https://jack.wrenn.fyi/blog/private-trait-methods/
            // https://www.reddit.com/r/rust/comments/12cj6as/comment/jf21zsm/
            for generic_param in &func.generics.params {
                match &generic_param.kind {
                    rustdoc_types::GenericParamDefKind::Type {
                        bounds, default, ..
                    } => {
                        // If the generic parameter has a default, it can't be used to seal.
                        if default.is_none() {
                            for bound in bounds {
                                if is_generic_type_bound_sealed(indexed_crate, bound) {
                                    return true;
                                }
                            }
                        }
                    }
                    _ => continue,
                }
            }
        }
    }

    false
}

fn is_generic_type_bound_sealed<'a>(
    indexed_crate: &IndexedCrate<'a>,
    bound: &'a rustdoc_types::GenericBound,
) -> bool {
    match bound {
        rustdoc_types::GenericBound::TraitBound {
            trait_: trait_path, ..
        } => {
            // For the bound to be sealing, it needs to:
            // - point to a pub-in-priv trait in this crate.
            // - all supertraits of the pub-in-priv trait must also be pub-in-priv
            let Some(item) = indexed_crate.inner.index.get(&trait_path.id) else {
                // Not an item in this crate.
                return false;
            };
            if !is_pub_in_priv_item(indexed_crate, &item.id) {
                return false;
            }

            let trait_item = unwrap_trait(item);

            // Check all supertraits to ensure they are pub-in-priv as well.
            for trait_bounds in &trait_item.bounds {
                if let rustdoc_types::GenericBound::TraitBound {
                    trait_: inner_trait_path,
                    ..
                } = trait_bounds
                {
                    if !is_local_pub_in_priv_path(indexed_crate, inner_trait_path) {
                        return false;
                    }
                }
            }

            true
        }
        _ => false,
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
