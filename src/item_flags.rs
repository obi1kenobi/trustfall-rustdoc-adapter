#[cfg(not(feature = "rustc-hash"))]
use std::collections::HashMap;

#[cfg(feature = "rustc-hash")]
use rustc_hash::FxHashMap as HashMap;

use rustdoc_types::{Id, Item};

use crate::{
    attributes::Attribute,
    indexed_crate::{Modifiers, Path},
    sealed_trait,
};

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub(crate) struct ItemFlag(u8);

impl Default for ItemFlag {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// Layout:
/// +---+---+---+---+---+---+---+---+
/// | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
/// +---+---+---+---+---+---+---+---+
/// | D | S | B | R | R | R | H | P |
/// +---+---+---+---+---+---+---+---+
///
/// Key:
/// P = pub reachable item: an item with a publicly importable name,
///     or a public API associated item of such an item
/// H = item publicly importable via a `doc(hidden)` path or an associated item of such an item,
///     or a `doc(hidden)` associated item of a pub reachable item
/// R = reserved for future use
/// B = trait with blanket impls (`impl<T> Trait for T`, possibly with some bounds on `T`)
/// S = sealed trait, external crates cannot provide an impl for this trait
/// D = `doc(hidden)`-sealed trait, providing an impl requires using `doc(hidden)` items
impl ItemFlag {
    const PUB_REACHABLE: Self = Self(1 << 0);
    const DOC_HIDDEN_REACHABLE: Self = Self(1 << 1);
    const TRAIT_BLANKET_IMPLS: Self = Self(1 << 5);
    const TRAIT_SEALED: Self = Self(1 << 6);
    const TRAIT_DOC_HIDDEN_SEALED: Self = Self(1 << 7);

    #[inline]
    pub(crate) fn new() -> Self {
        Self(0)
    }

    /// Whether the item is reachable from another crate, ignoring public API considerations.
    ///
    /// Items that aren't reachable simply cannot be used in a downstream crate.
    /// Their visibility does not allow them to be accessed, and doing so is a hard compiler error.
    #[inline]
    pub(crate) fn is_reachable(&self) -> bool {
        (self.0 & (Self::PUB_REACHABLE.0 | Self::DOC_HIDDEN_REACHABLE.0)) != 0
    }

    /// Whether the item is reachable from another crate via the public API of this crate.
    ///
    /// This means the item is importable, or is a public API component of an importable item.
    /// For example: a `pub trait` at the root `lib.rs` file, and a `pub fn` within the trait,
    /// are both items where [`Self::is_pub_reachable()`] returns `true`.
    ///
    /// An example where [`Self::is_pub_reachable()`] returns `false` but [`Self::is_reachable()`]
    /// would return `true` is a `#[doc(hidden)]` non-deprecated item. Such an item is not part of
    /// the public API, even though it is possible to access from outside its crate.
    #[inline]
    pub(crate) fn is_pub_reachable(&self) -> bool {
        (self.0 & Self::PUB_REACHABLE.0) != 0
    }

    /// Whether the item is reachable via non-public API in this crate.
    ///
    /// This is not mutually-exclusive with [`Self::is_pub_reachable()`]!
    /// For example, it's possible for an item to be both public API and non-public API
    /// simultaneously at different paths, and have both [`Self::is_non_pub_api_reachable()`] and
    /// [`Self::is_pub_reachable()`] return `true`:
    /// ```no_run
    /// #[doc(hidden)]
    /// pub mod hidden {
    ///     // The item path `this_crate::hidden::Example` is accessible, but not public API.
    ///     pub struct Example;
    /// }
    ///
    /// // The item path `this_crate::Example` is public API, and not hidden.
    /// pub use hidden::Example;
    /// ```
    #[inline]
    pub(crate) fn is_non_pub_api_reachable(&self) -> bool {
        (self.0 & Self::DOC_HIDDEN_REACHABLE.0) != 0
    }

    #[inline]
    pub(crate) fn set_pub_reachable(&mut self) {
        self.0 |= Self::PUB_REACHABLE.0;
    }

    #[inline]
    pub(crate) fn set_doc_hidden_reachable(&mut self) {
        self.0 |= Self::DOC_HIDDEN_REACHABLE.0;
    }

    /// Whether the trait has impls like `impl<T> TheTrait for T`, with optional bounds on `T`.
    #[inline]
    pub(crate) fn trait_has_blanket_impls(&self) -> bool {
        (self.0 & Self::TRAIT_BLANKET_IMPLS.0) != 0
    }

    /// Whether the trait is unconditionally sealed: a downstream crate cannot provide its own impl.
    ///
    /// Attempting to implement the trait in a downstream crate is guaranteed to be a compile error.
    #[inline]
    pub(crate) fn is_unconditionally_sealed(&self) -> bool {
        (self.0 & Self::TRAIT_SEALED.0) != 0
    }

    /// Whether the trait is sealed only in public API: impls of the trait rely on non-public API.
    ///
    /// Implementations of the trait in a downstream crate are forced to rely on non-public API,
    /// meaning they are not covered by SemVer stability guarantees and may suffer breakage.
    ///
    /// If the trait is unconditionally sealed, this method returns `false`. In other words,
    /// at most one of [`Self::is_only_pub_api_sealed()`] and
    /// [`Self::is_unconditionally_sealed()`] returns `true`.
    #[inline]
    pub(crate) fn is_only_pub_api_sealed(&self) -> bool {
        (self.0 & Self::TRAIT_DOC_HIDDEN_SEALED.0) != 0
    }

    /// Whether downstream crates can provide impls for this trait within public API.
    ///
    /// Such impls are then covered by SemVer stability guarantees.
    #[inline]
    pub(crate) fn is_pub_api_implementable(&self) -> bool {
        (self.0 & (Self::TRAIT_DOC_HIDDEN_SEALED.0 | Self::TRAIT_SEALED.0)) == 0
    }

    #[inline]
    pub(crate) fn set_trait_has_blanket_impls(&mut self) {
        self.0 |= Self::TRAIT_BLANKET_IMPLS.0;
    }

    #[inline]
    pub(crate) fn set_unconditionally_sealed(&mut self) {
        // Turn off the "public-API-sealed" bit, since sealed dominates.
        self.0 &= !Self::TRAIT_DOC_HIDDEN_SEALED.0;
        self.0 |= Self::TRAIT_SEALED.0;
    }

    #[inline]
    pub(crate) fn set_pub_api_sealed(&mut self) {
        if !self.is_unconditionally_sealed() {
            self.0 |= Self::TRAIT_DOC_HIDDEN_SEALED.0;
        }
    }

    #[inline]
    fn get_reachability(&self) -> Reachability {
        let mask = self.0 & (Self::PUB_REACHABLE.0 | Self::DOC_HIDDEN_REACHABLE.0);
        if mask == 0 {
            Reachability::Unreachable
        } else if mask == Self::DOC_HIDDEN_REACHABLE.0 {
            Reachability::NonPublicAPI
        } else {
            Reachability::PublicAPI
        }
    }

    #[inline]
    fn apply_reachability(&mut self, reachability: Reachability) {
        match reachability {
            Reachability::Unreachable => {}
            Reachability::NonPublicAPI => self.set_doc_hidden_reachable(),
            Reachability::PublicAPI => self.set_pub_reachable(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Reachability {
    Unreachable,
    NonPublicAPI,
    PublicAPI,
}

impl Reachability {
    fn from_parent(parent_reachability: Self, item: &Item) -> Self {
        match parent_reachability {
            Reachability::Unreachable => parent_reachability,
            Reachability::NonPublicAPI => match item.visibility {
                rustdoc_types::Visibility::Public | rustdoc_types::Visibility::Default => {
                    Reachability::NonPublicAPI
                }
                rustdoc_types::Visibility::Crate | rustdoc_types::Visibility::Restricted { .. } => {
                    Reachability::Unreachable
                }
            },
            Reachability::PublicAPI => match item.visibility {
                rustdoc_types::Visibility::Public | rustdoc_types::Visibility::Default => {
                    if item.deprecation.is_none()
                        && item
                            .attrs
                            .iter()
                            .any(|attr| Attribute::is_doc_hidden(attr.as_str()))
                    {
                        Reachability::NonPublicAPI
                    } else {
                        Reachability::PublicAPI
                    }
                }
                rustdoc_types::Visibility::Crate | rustdoc_types::Visibility::Restricted { .. } => {
                    Reachability::Unreachable
                }
            },
        }
    }
}

pub(crate) fn build_flags_index(
    index: &HashMap<Id, Item>,
    imports_index: &HashMap<Path<'_>, Vec<(&Item, Modifiers)>>,
) -> HashMap<Id, ItemFlag> {
    let mut flags: HashMap<Id, ItemFlag> =
        index.keys().map(|id| (*id, Default::default())).collect();

    // First, initialize the flags of all top-level importable items.
    imports_index
        .values()
        .flatten()
        .for_each(|(item, modifiers)| {
            let flag = flags.entry(item.id).or_default();
            if !modifiers.deprecated && modifiers.doc_hidden {
                flag.set_doc_hidden_reachable();
            } else {
                flag.set_pub_reachable();
            }
        });

    // Then, traverse the children of all top-level items to set the flags of those child items.
    index.values().for_each(|item| {
        let parent_reachability = flags[&item.id].get_reachability();
        match &item.inner {
            rustdoc_types::ItemEnum::Union(inner) => {
                set_field_flags_index(
                    inner.fields.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
                set_impl_flags_index(
                    index,
                    inner.impls.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
            }
            rustdoc_types::ItemEnum::Struct(inner) => {
                match &inner.kind {
                    rustdoc_types::StructKind::Unit => {}
                    rustdoc_types::StructKind::Tuple(ids) => {
                        set_field_flags_index(
                            ids.iter()
                                .filter_map(|x| x.as_ref())
                                .filter_map(|id| index.get(id)),
                            &mut flags,
                            parent_reachability,
                        );
                    }
                    rustdoc_types::StructKind::Plain { fields, .. } => {
                        set_field_flags_index(
                            fields.iter().filter_map(|id| index.get(id)),
                            &mut flags,
                            parent_reachability,
                        );
                    }
                }

                set_impl_flags_index(
                    index,
                    inner.impls.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
            }
            rustdoc_types::ItemEnum::Enum(inner) => {
                set_variant_flags_index(
                    index,
                    inner.variants.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
                set_impl_flags_index(
                    index,
                    inner.impls.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
            }
            rustdoc_types::ItemEnum::Trait(inner) => {
                set_assoc_item_flags_index(
                    inner.items.iter().filter_map(|id| index.get(id)),
                    &mut flags,
                    parent_reachability,
                );
            }
            _ => {}
        }
    });

    sealed_trait::compute_trait_flags(index, &mut flags);

    flags
}

fn set_field_flags_index<'a>(
    fields: impl Iterator<Item = &'a Item>,
    flags: &mut HashMap<Id, ItemFlag>,
    parent_reachability: Reachability,
) {
    fields.for_each(|item| {
        let reachability = Reachability::from_parent(parent_reachability, item);
        let flag = flags.get_mut(&item.id).expect("missing flag for item");
        flag.apply_reachability(reachability);
    })
}

fn set_variant_flags_index<'a>(
    index: &HashMap<Id, Item>,
    variants: impl Iterator<Item = &'a Item>,
    flags: &mut HashMap<Id, ItemFlag>,
    parent_reachability: Reachability,
) {
    variants.for_each(|item| {
        let reachability = Reachability::from_parent(parent_reachability, item);
        let flag = flags.get_mut(&item.id).expect("missing flag for item");
        flag.apply_reachability(reachability);

        match &item.inner {
            rustdoc_types::ItemEnum::Variant(variant) => match &variant.kind {
                rustdoc_types::VariantKind::Plain => {}
                rustdoc_types::VariantKind::Tuple(ids) => {
                    set_field_flags_index(
                        ids.iter()
                            .filter_map(|x| x.as_ref())
                            .filter_map(|id| index.get(id)),
                        flags,
                        reachability,
                    );
                }
                rustdoc_types::VariantKind::Struct { fields, .. } => {
                    set_field_flags_index(
                        fields.iter().filter_map(|id| index.get(id)),
                        flags,
                        reachability,
                    );
                }
            },
            _ => unreachable!("not a variant item: {item:?}"),
        }
    })
}

fn set_impl_flags_index<'a>(
    index: &HashMap<Id, Item>,
    impls: impl Iterator<Item = &'a Item>,
    flags: &mut HashMap<Id, ItemFlag>,
    parent_reachability: Reachability,
) {
    impls.for_each(|item| {
        let reachability = Reachability::from_parent(parent_reachability, item);
        let flag = flags.get_mut(&item.id).expect("missing flag for item");
        flag.apply_reachability(reachability);

        match &item.inner {
            rustdoc_types::ItemEnum::Impl(impl_inner) => {
                set_assoc_item_flags_index(
                    impl_inner.items.iter().filter_map(|id| index.get(id)),
                    flags,
                    reachability,
                );
            }
            _ => unreachable!("not an impl item: {item:?}"),
        }
    })
}

fn set_assoc_item_flags_index<'a>(
    assoc_items: impl Iterator<Item = &'a Item>,
    flags: &mut HashMap<Id, ItemFlag>,
    parent_reachability: Reachability,
) {
    assoc_items.for_each(|item| {
        let reachability = Reachability::from_parent(parent_reachability, item);
        let flag = flags.get_mut(&item.id).expect("missing flag for item");
        flag.apply_reachability(reachability);
    })
}
