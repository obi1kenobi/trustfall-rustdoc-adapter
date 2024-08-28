use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap, HashSet},
};

#[cfg(feature = "rayon")]
use rayon::prelude::*;
use rustdoc_types::{Crate, Id, Item};

use crate::{adapter::supported_item_kind, sealed_trait, visibility_tracker::VisibilityTracker};

/// The rustdoc for a crate, together with associated indexed data to speed up common operations.
///
/// Besides the parsed rustdoc, it also contains some manually-inlined `rustdoc_types::Trait`s
/// of the most common built-in traits.
/// This is a temporary step, until we're able to combine rustdocs of multiple crates.
#[derive(Debug, Clone)]
pub struct IndexedCrate<'a> {
    pub(crate) inner: &'a Crate,

    /// Track which items are publicly visible and under which names.
    pub(crate) visibility_tracker: VisibilityTracker<'a>,

    /// index: importable name (in any namespace) -> list of items under that name
    pub(crate) imports_index: Option<HashMap<Path<'a>, Vec<(&'a Item, Modifiers)>>>,

    /// index: impl owner + impl'd item name -> list of (impl itself, the named item))
    pub(crate) impl_index: Option<HashMap<ImplEntry<'a>, Vec<(&'a Item, &'a Item)>>>,

    /// Trait items defined in external crates are not present in the `inner: &Crate` field,
    /// even if they are implemented by a type in that crate. This also includes
    /// Rust's built-in traits like `Debug, Send, Eq` etc.
    ///
    /// This change is approximately as of rustdoc v23,
    /// in <https://github.com/rust-lang/rust/pull/105182>
    ///
    /// As a temporary workaround, we manually create the trait items
    /// for the most common Rust built-in traits and link to those items
    /// as if they were still part of the rustdoc JSON file.
    ///
    /// A more complete future solution may generate multiple crates' rustdoc JSON
    /// and link to the external crate's trait items as necessary.
    pub(crate) manually_inlined_builtin_traits: HashMap<Id, Item>,
}

/// Map a Key to a List (Vec) of values
///
/// It also has some nice operations for pushing a value to the list, or extending the list with
/// many values.
struct MapList<K, V>(HashMap<K, Vec<V>>);

#[cfg(feature = "rayon")]
impl<K: std::cmp::Eq + std::hash::Hash + Send, V: Send> FromParallelIterator<(K, V)>
    for MapList<K, V>
{
    #[inline]
    fn from_par_iter<I>(par_iter: I) -> Self
    where
        I: IntoParallelIterator<Item = (K, V)>,
    {
        par_iter
            .into_par_iter()
            .fold(Self::new, |mut map, (key, value)| {
                map.insert(key, value);
                map
            })
            // Reduce left is faster than reduce right (about 19% less time in our benchmarks)
            .reduce(Self::new, |mut l, r| {
                l.merge(r);
                l
            })
    }
}

impl<K: std::cmp::Eq + std::hash::Hash, V> FromIterator<(K, V)> for MapList<K, V> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        // We could use Iterator::size_hint here to preallocate some space, but I couldn't measure
        // a perf inprovement from that.
        let mut map = Self::new();
        for (key, value) in iter {
            map.insert(key, value);
        }
        map
    }
}

impl<K: std::cmp::Eq + std::hash::Hash, V> Extend<(K, V)> for MapList<K, V> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        // We could use Iterator::size_hint here to reserve some space, but I measured a 2%-3%
        // regression when doing that.
        for (key, value) in iter.into_iter() {
            self.insert(key, value);
        }
    }
}

impl<K: std::cmp::Eq + std::hash::Hash, V> MapList<K, V> {
    #[inline]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    #[inline]
    pub fn into_inner(self) -> HashMap<K, Vec<V>> {
        self.0
    }

    #[inline]
    pub fn insert(&mut self, key: K, value: V) {
        match self.0.entry(key) {
            Entry::Occupied(mut entry) => entry.get_mut().push(value),
            Entry::Vacant(entry) => {
                entry.insert(vec![value]);
            }
        }
    }

    #[inline]
    #[cfg(feature = "rayon")]
    pub fn insert_many(&mut self, key: K, mut value: Vec<V>) {
        match self.0.entry(key) {
            Entry::Occupied(mut entry) => entry.get_mut().append(&mut value),
            Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
    }

    #[inline]
    #[cfg(feature = "rayon")]
    pub fn merge(&mut self, other: Self) {
        self.0.reserve(other.0.len());
        for (key, value) in other.0 {
            self.insert_many(key, value);
        }
    }
}

/// Build the impl index
///
/// When compiled using the `rayon` feature, build it in parallel. Specifically, this paralelizes
/// the work of gathering all of the impls for the items in the index.
fn build_impl_index(index: &HashMap<Id, Item>) -> MapList<ImplEntry<'_>, (&Item, &Item)> {
    #[cfg(feature = "rayon")]
    let iter = index.par_iter();
    #[cfg(not(feature = "rayon"))]
    let iter = index.iter();
    iter.filter_map(|(id, item)| {
        let impls = match &item.inner {
            rustdoc_types::ItemEnum::Struct(s) => s.impls.as_slice(),
            rustdoc_types::ItemEnum::Enum(e) => e.impls.as_slice(),
            rustdoc_types::ItemEnum::Union(u) => u.impls.as_slice(),
            _ => return None,
        };

        #[cfg(feature = "rayon")]
        let iter = impls.par_iter();
        #[cfg(not(feature = "rayon"))]
        let iter = impls.iter();

        Some((id, iter.filter_map(|impl_id| index.get(impl_id))))
    })
    .flat_map(|(id, impl_items)| {
        impl_items.flat_map(move |impl_item| {
            let impl_inner = match &impl_item.inner {
                rustdoc_types::ItemEnum::Impl(impl_inner) => impl_inner,
                _ => unreachable!("expected impl but got another item type: {impl_item:?}"),
            };
            let trait_provided_methods: HashSet<_> = impl_inner
                .provided_trait_methods
                .iter()
                .map(|x| x.as_str())
                .collect();

            let trait_items = impl_inner
                .trait_
                .as_ref()
                .and_then(|trait_path| index.get(&trait_path.id))
                .map(move |trait_item| {
                    if let rustdoc_types::ItemEnum::Trait(trait_item) = &trait_item.inner {
                        trait_item.items.as_slice()
                    } else {
                        &[]
                    }
                })
                .unwrap_or(&[]);

            #[cfg(feature = "rayon")]
            let trait_items = trait_items.par_iter();
            #[cfg(not(feature = "rayon"))]
            let trait_items = trait_items.iter();

            let trait_provided_items = trait_items
                .filter_map(|id| index.get(id))
                .filter(move |item| {
                    item.name
                        .as_deref()
                        .map(|name| trait_provided_methods.contains(name))
                        .unwrap_or_default()
                })
                .map(move |provided_item| {
                    (
                        ImplEntry::new(
                            id,
                            provided_item
                                .name
                                .as_deref()
                                .expect("item should have had a name"),
                        ),
                        (impl_item, provided_item),
                    )
                });

            #[cfg(feature = "rayon")]
            let impl_items = impl_inner.items.par_iter();
            #[cfg(not(feature = "rayon"))]
            let impl_items = impl_inner.items.iter();

            impl_items
                .filter_map(move |item_id| {
                    let item = index.get(item_id)?;
                    let item_name = item.name.as_deref()?;
                    Some((ImplEntry::new(id, item_name), (impl_item, item)))
                })
                .chain(trait_provided_items)
        })
    })
    .collect()
}

impl<'a> IndexedCrate<'a> {
    pub fn new(crate_: &'a Crate) -> Self {
        let mut value = Self {
            inner: crate_,
            visibility_tracker: VisibilityTracker::from_crate(crate_),
            manually_inlined_builtin_traits: create_manually_inlined_builtin_traits(crate_),
            imports_index: None,
            impl_index: None,
        };

        // Build the imports index
        //
        // This is inlined because we need access to `value`, but `value` is not a valid
        // `IndexedCrate` yet. Do not extract into a separate function.
        #[cfg(feature = "rayon")]
        let iter = crate_.index.par_iter();
        #[cfg(not(feature = "rayon"))]
        let iter = crate_.index.iter();

        value.imports_index = Some(
            iter.filter_map(|(_id, item)| {
                if !supported_item_kind(item) {
                    return None;
                }
                let importable_paths = value.publicly_importable_names(&item.id);

                #[cfg(feature = "rayon")]
                let iter = importable_paths.into_par_iter();
                #[cfg(not(feature = "rayon"))]
                let iter = importable_paths.into_iter();

                Some(iter.map(move |importable_path| {
                    (importable_path.path, (item, importable_path.modifiers))
                }))
            })
            .flatten()
            .collect::<MapList<_, _>>()
            .into_inner(),
        );

        value.impl_index = Some(build_impl_index(&crate_.index).into_inner());

        value
    }

    /// Return all the paths with which the given item can be imported from this crate.
    pub fn publicly_importable_names(&self, id: &'a Id) -> Vec<ImportablePath<'a>> {
        if self.inner.index.contains_key(id) {
            self.visibility_tracker
                .collect_publicly_importable_names(id)
        } else {
            Default::default()
        }
    }

    /// Return `true` if our analysis indicates the trait is sealed, and `false` otherwise.
    ///
    /// Our analysis is conservative: it has false-negatives but no false-positives.
    /// If this method returns `true`, the trait is *definitely* sealed or else you've found a bug.
    /// It may be possible to construct traits that *technically* are sealed for which our analysis
    /// returns `false`.
    ///
    /// The goal of this method is to reflect author intent, not technicalities.
    /// When Rustaceans seal traits on purpose, they do so with a limited number of techniques
    /// that are well-defined and immediately recognizable to readers in the community:
    /// <https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/>
    ///
    /// The analysis here looks for such techniques, which are always applied at the type signature
    /// level. It does not inspect function bodies or do interprocedural analysis.
    ///
    /// ## Panics
    ///
    /// This method will panic if the provided `id` is not an item in this crate,
    /// or does not correspond to a trait in this crate.
    ///
    /// ## Re-entrancy
    ///
    /// This method is re-entrant: calling it may cause additional calls to itself, inquiring about
    /// the sealed-ness of a trait's supertraits.
    ///
    /// We rely on rustc to reject supertrait cycles in order to prevent infinite loops.
    /// Here's a supertrait cycle that must be rejected by rustc:
    /// ```compile_fail
    /// pub trait First: Third {}
    ///
    /// pub trait Second: First {}
    ///
    /// pub trait Third: Second {}
    /// ```
    pub fn is_trait_sealed(&self, id: &'a Id) -> bool {
        let trait_item = &self.inner.index[id];
        sealed_trait::is_trait_sealed(self, trait_item)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub struct Path<'a> {
    pub(crate) components: Vec<&'a str>,
}

impl<'a> Path<'a> {
    fn new(components: Vec<&'a str>) -> Self {
        Self { components }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub struct Modifiers {
    pub(crate) doc_hidden: bool,
    pub(crate) deprecated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub struct ImportablePath<'a> {
    pub(crate) path: Path<'a>,
    pub(crate) modifiers: Modifiers,
}

impl<'a> ImportablePath<'a> {
    pub(crate) fn new(components: Vec<&'a str>, doc_hidden: bool, deprecated: bool) -> Self {
        Self {
            path: Path::new(components),
            modifiers: Modifiers {
                doc_hidden,
                deprecated,
            },
        }
    }

    pub(crate) fn public_api(&self) -> bool {
        self.modifiers.deprecated || !self.modifiers.doc_hidden
    }
}

impl<'a: 'b, 'b> Borrow<[&'b str]> for Path<'a> {
    fn borrow(&self) -> &[&'b str] {
        &self.components
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImplEntry<'a> {
    /// Tuple of:
    /// - the Id of the struct/enum/union that owns the item,
    /// - the name of the item in the owner's `impl` block.
    ///
    /// Stored as a tuple to make the `Borrow` impl work.
    pub(crate) data: (&'a Id, &'a str),
}

impl<'a> ImplEntry<'a> {
    #[inline]
    fn new(owner_id: &'a Id, item_name: &'a str) -> Self {
        Self {
            data: (owner_id, item_name),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn owner_id(&self) -> &'a Id {
        self.data.0
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn item_name(&self) -> &'a str {
        self.data.1
    }
}

impl<'a: 'b, 'b> Borrow<(&'b Id, &'b str)> for ImplEntry<'a> {
    fn borrow(&self) -> &(&'b Id, &'b str) {
        &(self.data)
    }
}

#[derive(Debug)]
struct ManualTraitItem {
    name: &'static str,
    is_auto: bool,
    is_unsafe: bool,
}

/// Limiting the creation of manually inlined traits to only those that are used by the lints.
/// There are other foreign traits, but it is not obvious how the manually inlined traits
/// should look like for them.
const MANUAL_TRAIT_ITEMS: [ManualTraitItem; 14] = [
    ManualTraitItem {
        name: "Debug",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Clone",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Copy",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "PartialOrd",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Ord",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "PartialEq",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Eq",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Hash",
        is_auto: false,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Send",
        is_auto: true,
        is_unsafe: true,
    },
    ManualTraitItem {
        name: "Sync",
        is_auto: true,
        is_unsafe: true,
    },
    ManualTraitItem {
        name: "Unpin",
        is_auto: true,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "RefUnwindSafe",
        is_auto: true,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "UnwindSafe",
        is_auto: true,
        is_unsafe: false,
    },
    ManualTraitItem {
        name: "Sized",
        is_auto: false,
        is_unsafe: false,
    },
];

fn new_trait(manual_trait_item: &ManualTraitItem, id: Id, crate_id: u32) -> Item {
    Item {
        id,
        crate_id,
        name: Some(manual_trait_item.name.to_string()),
        span: None,
        visibility: rustdoc_types::Visibility::Public,
        docs: None,
        links: HashMap::new(),
        attrs: Vec::new(),
        deprecation: None,
        inner: rustdoc_types::ItemEnum::Trait(rustdoc_types::Trait {
            is_auto: manual_trait_item.is_auto,
            is_unsafe: manual_trait_item.is_unsafe,
            is_object_safe: matches!(
                manual_trait_item.name,
                "Debug"
                    | "PartialEq"
                    | "PartialOrd"
                    | "Send"
                    | "Sync"
                    | "Unpin"
                    | "UnwindSafe"
                    | "RefUnwindSafe"
            ),
            // The `item`, `generics`, `bounds` and `implementations`
            // are not currently present in the schema,
            // so it is safe to fill them with empty containers,
            // even though some traits in reality have some values in them.
            items: Vec::new(),
            generics: rustdoc_types::Generics {
                params: Vec::new(),
                where_predicates: Vec::new(),
            },
            bounds: Vec::new(),
            implementations: Vec::new(),
        }),
    }
}

fn create_manually_inlined_builtin_traits(crate_: &Crate) -> HashMap<Id, Item> {
    let paths = crate_
        .index
        .values()
        .map(|item| &item.inner)
        .filter_map(|item_enum| match item_enum {
            rustdoc_types::ItemEnum::Impl(impl_) => Some(impl_),
            _ => None,
        })
        .filter_map(|impl_| impl_.trait_.as_ref());

    paths
        .filter_map(|path| {
            MANUAL_TRAIT_ITEMS
                .iter()
                .find(|manual| manual.name == path.name)
                .and_then(|manual| {
                    crate_.paths.get(&path.id).map(|item_summary| {
                        (
                            path.id.clone(),
                            new_trait(manual, path.id.clone(), item_summary.crate_id),
                        )
                    })
                })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use rustdoc_types::{Crate, Id};

    use crate::{test_util::load_pregenerated_rustdoc, ImportablePath, IndexedCrate};

    fn find_item_id<'a>(crate_: &'a Crate, name: &str) -> &'a Id {
        crate_
            .index
            .iter()
            .filter_map(|(id, item)| (item.name.as_deref() == Some(name)).then_some(id))
            .exactly_one()
            .expect("exactly one matching name")
    }

    /// Ensure that methods, consts, and fields within structs are not importable.
    #[test]
    fn structs_are_not_modules() {
        let rustdoc = load_pregenerated_rustdoc("structs_are_not_modules");
        let indexed_crate = IndexedCrate::new(&rustdoc);

        let top_level_function = find_item_id(&rustdoc, "top_level_function");
        let method = find_item_id(&rustdoc, "method");
        let associated_fn = find_item_id(&rustdoc, "associated_fn");
        let field = find_item_id(&rustdoc, "field");
        let const_item = find_item_id(&rustdoc, "THE_ANSWER");

        // All the items are public.
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(top_level_function));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(method));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(associated_fn));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(field));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(const_item));

        // But only `top_level_function` is importable.
        assert_eq!(
            vec![ImportablePath::new(
                vec!["structs_are_not_modules", "top_level_function"],
                false,
                false,
            )],
            indexed_crate.publicly_importable_names(top_level_function)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(method)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(associated_fn)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(field)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(const_item)
        );
    }

    /// Ensure that methods and consts within enums are not importable.
    /// However, enum variants are the exception: they are importable!
    #[test]
    fn enums_are_not_modules() {
        let rustdoc = load_pregenerated_rustdoc("enums_are_not_modules");
        let indexed_crate = IndexedCrate::new(&rustdoc);

        let top_level_function = find_item_id(&rustdoc, "top_level_function");
        let variant = find_item_id(&rustdoc, "Variant");
        let method = find_item_id(&rustdoc, "method");
        let associated_fn = find_item_id(&rustdoc, "associated_fn");
        let const_item = find_item_id(&rustdoc, "THE_ANSWER");

        // All the items are public.
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(top_level_function));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(variant));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(method));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(associated_fn));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(const_item));

        // But only `top_level_function` and `Foo::variant` is importable.
        assert_eq!(
            vec![ImportablePath::new(
                vec!["enums_are_not_modules", "top_level_function"],
                false,
                false,
            )],
            indexed_crate.publicly_importable_names(top_level_function)
        );
        assert_eq!(
            vec![ImportablePath::new(
                vec!["enums_are_not_modules", "Foo", "Variant"],
                false,
                false,
            )],
            indexed_crate.publicly_importable_names(variant)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(method)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(associated_fn)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(const_item)
        );
    }

    /// Ensure that methods, consts, and fields within unions are not importable.
    #[test]
    fn unions_are_not_modules() {
        let rustdoc = load_pregenerated_rustdoc("unions_are_not_modules");
        let indexed_crate = IndexedCrate::new(&rustdoc);

        let top_level_function = find_item_id(&rustdoc, "top_level_function");
        let method = find_item_id(&rustdoc, "method");
        let associated_fn = find_item_id(&rustdoc, "associated_fn");
        let left_field = find_item_id(&rustdoc, "left");
        let right_field = find_item_id(&rustdoc, "right");
        let const_item = find_item_id(&rustdoc, "THE_ANSWER");

        // All the items are public.
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(top_level_function));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(method));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(associated_fn));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(left_field));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(right_field));
        assert!(indexed_crate
            .visibility_tracker
            .visible_parent_ids()
            .contains_key(const_item));

        // But only `top_level_function` is importable.
        assert_eq!(
            vec![ImportablePath::new(
                vec!["unions_are_not_modules", "top_level_function"],
                false,
                false,
            )],
            indexed_crate.publicly_importable_names(top_level_function)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(method)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(associated_fn)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(left_field)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(right_field)
        );
        assert_eq!(
            Vec::<ImportablePath<'_>>::new(),
            indexed_crate.publicly_importable_names(const_item)
        );
    }

    mod reexports {
        use std::collections::{BTreeMap, BTreeSet};

        use itertools::Itertools;
        use maplit::{btreemap, btreeset};
        use rustdoc_types::{ItemEnum, Visibility};

        use crate::{test_util::load_pregenerated_rustdoc, ImportablePath, IndexedCrate};

        fn assert_exported_items_match(
            test_crate: &str,
            expected_items: &BTreeMap<&str, BTreeSet<&str>>,
        ) {
            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            for (&expected_item_name, expected_importable_paths) in expected_items {
                assert!(
                    !expected_item_name.contains(':'),
                    "only direct item names can be checked at the moment: {expected_item_name}"
                );

                let item_id_candidates = rustdoc
                    .index
                    .iter()
                    .filter_map(|(id, item)| {
                        (item.name.as_deref() == Some(expected_item_name)).then_some(id)
                    })
                    .collect_vec();
                if item_id_candidates.len() != 1 {
                    panic!(
                        "Expected to find exactly one item with name {expected_item_name}, \
                        but found these matching IDs: {item_id_candidates:?}"
                    );
                }
                let item_id = item_id_candidates[0];
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();
                let deduplicated_actual_items: BTreeSet<_> =
                    actual_items.iter().map(|x| x.as_str()).collect();
                assert_eq!(
                    actual_items.len(),
                    deduplicated_actual_items.len(),
                    "duplicates found: {actual_items:?}"
                );

                assert_eq!(
                    expected_importable_paths, &deduplicated_actual_items,
                    "mismatch for item name {expected_item_name}",
                );
            }
        }

        /// Allows testing for items with overlapping names, such as a function and a type
        /// with the same name (which Rust considers in separate namespaces).
        fn assert_duplicated_exported_items_match(
            test_crate: &str,
            expected_items_and_counts: &BTreeMap<&str, (usize, BTreeSet<&str>)>,
        ) {
            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            for (&expected_item_name, (expected_count, expected_importable_paths)) in
                expected_items_and_counts
            {
                assert!(
                    !expected_item_name.contains(':'),
                    "only direct item names can be checked at the moment: {expected_item_name}"
                );

                let item_id_candidates = rustdoc
                    .index
                    .iter()
                    .filter_map(|(id, item)| {
                        (item.name.as_deref() == Some(expected_item_name)).then_some(id)
                    })
                    .collect_vec();
                if item_id_candidates.len() != *expected_count {
                    panic!(
                        "Expected to find exactly {expected_count} items with name \
                        {expected_item_name}, but found these matching IDs: {item_id_candidates:?}"
                    );
                }
                for item_id in item_id_candidates {
                    let actual_items: Vec<_> = indexed_crate
                        .publicly_importable_names(item_id)
                        .into_iter()
                        .map(|importable| importable.path.components.into_iter().join("::"))
                        .collect();
                    let deduplicated_actual_items: BTreeSet<_> =
                        actual_items.iter().map(|x| x.as_str()).collect();
                    assert_eq!(
                        actual_items.len(),
                        deduplicated_actual_items.len(),
                        "duplicates found: {actual_items:?}"
                    );
                    assert_eq!(expected_importable_paths, &deduplicated_actual_items);
                }
            }
        }

        #[test]
        fn pub_inside_pub_crate_mod() {
            let test_crate = "pub_inside_pub_crate_mod";
            let expected_items = btreemap! {
                "Foo" => btreeset![],
                "Bar" => btreeset![
                    "pub_inside_pub_crate_mod::Bar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn reexport() {
            let test_crate = "reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "reexport::foo",
                    "reexport::inner::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn reexport_from_private_module() {
            let test_crate = "reexport_from_private_module";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "reexport_from_private_module::foo",
                ],
                "Bar" => btreeset![
                    "reexport_from_private_module::Bar",
                ],
                "Baz" => btreeset![
                    "reexport_from_private_module::nested::Baz",
                ],
                "quux" => btreeset![
                    "reexport_from_private_module::quux",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn renaming_reexport() {
            let test_crate = "renaming_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "renaming_reexport::bar",
                    "renaming_reexport::inner::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn renaming_reexport_of_reexport() {
            let test_crate = "renaming_reexport_of_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "renaming_reexport_of_reexport::bar",
                    "renaming_reexport_of_reexport::foo",
                    "renaming_reexport_of_reexport::inner::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn renaming_mod_reexport() {
            let test_crate = "renaming_mod_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "renaming_mod_reexport::inner::a::foo",
                    "renaming_mod_reexport::inner::b::foo",
                    "renaming_mod_reexport::direct::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_reexport() {
            let test_crate = "glob_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "glob_reexport::foo",
                    "glob_reexport::inner::foo",
                ],
                "Bar" => btreeset![
                    "glob_reexport::Bar",
                    "glob_reexport::inner::Bar",
                ],
                "nested" => btreeset![
                    "glob_reexport::nested",
                ],
                "Baz" => btreeset![
                    "glob_reexport::Baz",
                ],
                "First" => btreeset![
                    "glob_reexport::First",
                    "glob_reexport::Baz::First",
                ],
                "Second" => btreeset![
                    "glob_reexport::Second",
                    "glob_reexport::Baz::Second",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_of_glob_reexport() {
            let test_crate = "glob_of_glob_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "glob_of_glob_reexport::foo",
                ],
                "Bar" => btreeset![
                    "glob_of_glob_reexport::Bar",
                ],
                "Baz" => btreeset![
                    "glob_of_glob_reexport::Baz",
                ],
                "Onion" => btreeset![
                    "glob_of_glob_reexport::Onion",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_of_renamed_reexport() {
            let test_crate = "glob_of_renamed_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "glob_of_renamed_reexport::renamed_foo",
                ],
                "Bar" => btreeset![
                    "glob_of_renamed_reexport::RenamedBar",
                ],
                "First" => btreeset![
                    "glob_of_renamed_reexport::RenamedFirst",
                ],
                "Onion" => btreeset![
                    "glob_of_renamed_reexport::RenamedOnion",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_reexport_enum_variants() {
            let test_crate = "glob_reexport_enum_variants";
            let expected_items = btreemap! {
                "First" => btreeset![
                    "glob_reexport_enum_variants::First",
                ],
                "Second" => btreeset![
                    "glob_reexport_enum_variants::Second",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_reexport_cycle() {
            let test_crate = "glob_reexport_cycle";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    "glob_reexport_cycle::first::foo",
                    "glob_reexport_cycle::second::foo",
                ],
                "Bar" => btreeset![
                    "glob_reexport_cycle::first::Bar",
                    "glob_reexport_cycle::second::Bar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn infinite_recursive_reexport() {
            let test_crate = "infinite_recursive_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    // We don't want to expand all infinitely-many names here.
                    // We only return cycle-free paths, which are the following:
                    "infinite_recursive_reexport::foo",
                    "infinite_recursive_reexport::inner::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn infinite_indirect_recursive_reexport() {
            let test_crate = "infinite_indirect_recursive_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    // We don't want to expand all infinitely-many names here.
                    // We only return cycle-free paths, which are the following:
                    "infinite_indirect_recursive_reexport::foo",
                    "infinite_indirect_recursive_reexport::nested::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn infinite_corecursive_reexport() {
            let test_crate = "infinite_corecursive_reexport";
            let expected_items = btreemap! {
                "foo" => btreeset![
                    // We don't want to expand all infinitely-many names here.
                    // We only return cycle-free paths, which are the following:
                    "infinite_corecursive_reexport::a::foo",
                    "infinite_corecursive_reexport::b::a::foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_type_alias_reexport() {
            let test_crate = "pub_type_alias_reexport";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "pub_type_alias_reexport::Exported",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_reexport() {
            let test_crate = "pub_generic_type_alias_reexport";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    // Only `Exported` and `ExportedRenamedParams` are re-exports.
                    //
                    //`ExportedRenamedParams` renames the generic parameters
                    // but does not change their meaning.
                    //
                    // `ExportedWithDefaults` is not a re-export because it adds
                    //
                    // The other type aliases are not equivalent since they constrain
                    // some of the underlying type's generic parameters.
                    "pub_generic_type_alias_reexport::Exported",
                    "pub_generic_type_alias_reexport::ExportedRenamedParams",
                ],
                "Exported" => btreeset![
                    // The type alias itself is also a visible item.
                    "pub_generic_type_alias_reexport::Exported",
                ],
                "ExportedWithDefaults" => btreeset![
                    // The type alias itself is also a visible item.
                    "pub_generic_type_alias_reexport::ExportedWithDefaults",
                ],
                "ExportedRenamedParams" => btreeset![
                    // The type alias itself is also a visible item.
                    "pub_generic_type_alias_reexport::ExportedRenamedParams",
                ],
                "ExportedSpecificLifetime" => btreeset![
                    "pub_generic_type_alias_reexport::ExportedSpecificLifetime",
                ],
                "ExportedSpecificType" => btreeset![
                    "pub_generic_type_alias_reexport::ExportedSpecificType",
                ],
                "ExportedSpecificConst" => btreeset![
                    "pub_generic_type_alias_reexport::ExportedSpecificConst",
                ],
                "ExportedFullySpecified" => btreeset![
                    "pub_generic_type_alias_reexport::ExportedFullySpecified",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_shuffled_order() {
            let test_crate = "pub_generic_type_alias_shuffled_order";
            let expected_items = btreemap! {
                // The type aliases reverse the generic parameters' orders,
                // so they are not re-exports of the underlying types.
                "GenericFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::inner::GenericFoo",
                ],
                "LifetimeFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::inner::LifetimeFoo",
                ],
                "ConstFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::inner::ConstFoo",
                ],
                "ReversedGenericFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::ReversedGenericFoo",
                ],
                "ReversedLifetimeFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::ReversedLifetimeFoo",
                ],
                "ReversedConstFoo" => btreeset![
                    "pub_generic_type_alias_shuffled_order::ReversedConstFoo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_added_defaults() {
            let test_crate = "pub_generic_type_alias_added_defaults";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "pub_generic_type_alias_added_defaults::inner::Foo",
                ],
                "Bar" => btreeset![
                    "pub_generic_type_alias_added_defaults::inner::Bar",
                ],
                "DefaultFoo" => btreeset![
                    "pub_generic_type_alias_added_defaults::DefaultFoo",
                ],
                "DefaultBar" => btreeset![
                    "pub_generic_type_alias_added_defaults::DefaultBar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_changed_defaults() {
            let test_crate = "pub_generic_type_alias_changed_defaults";
            let expected_items = btreemap! {
                // The type aliases change the default values of the generic parameters,
                // so they are not re-exports of the underlying types.
                "Foo" => btreeset![
                    "pub_generic_type_alias_changed_defaults::inner::Foo",
                ],
                "Bar" => btreeset![
                    "pub_generic_type_alias_changed_defaults::inner::Bar",
                ],
                "ExportedWithoutTypeDefault" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithoutTypeDefault",
                ],
                "ExportedWithoutConstDefault" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithoutConstDefault",
                ],
                "ExportedWithoutDefaults" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithoutDefaults",
                ],
                "ExportedWithDifferentTypeDefault" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithDifferentTypeDefault",
                ],
                "ExportedWithDifferentConstDefault" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithDifferentConstDefault",
                ],
                "ExportedWithDifferentDefaults" => btreeset![
                    "pub_generic_type_alias_changed_defaults::ExportedWithDifferentDefaults",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_same_signature_but_not_equivalent() {
            let test_crate = "pub_generic_type_alias_same_signature_but_not_equivalent";
            let expected_items = btreemap! {
                "GenericFoo" => btreeset![
                    "pub_generic_type_alias_same_signature_but_not_equivalent::inner::GenericFoo",
                ],
                "ChangedFoo" => btreeset![
                    "pub_generic_type_alias_same_signature_but_not_equivalent::ChangedFoo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_type_alias_of_type_alias() {
            let test_crate = "pub_type_alias_of_type_alias";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "pub_type_alias_of_type_alias::inner::Foo",
                    "pub_type_alias_of_type_alias::inner::AliasedFoo",
                    "pub_type_alias_of_type_alias::ExportedFoo",
                ],
                "Bar" => btreeset![
                    "pub_type_alias_of_type_alias::inner::Bar",
                    "pub_type_alias_of_type_alias::inner::AliasedBar",
                    "pub_type_alias_of_type_alias::ExportedBar",
                ],
                "AliasedFoo" => btreeset![
                    "pub_type_alias_of_type_alias::inner::AliasedFoo",
                    "pub_type_alias_of_type_alias::ExportedFoo",
                ],
                "AliasedBar" => btreeset![
                    "pub_type_alias_of_type_alias::inner::AliasedBar",
                    "pub_type_alias_of_type_alias::ExportedBar",
                ],
                "ExportedFoo" => btreeset![
                    "pub_type_alias_of_type_alias::ExportedFoo",
                ],
                "ExportedBar" => btreeset![
                    "pub_type_alias_of_type_alias::ExportedBar",
                ],
                "DifferentLifetimeBar" => btreeset![
                    "pub_type_alias_of_type_alias::DifferentLifetimeBar",
                ],
                "DifferentGenericBar" => btreeset![
                    "pub_type_alias_of_type_alias::DifferentGenericBar",
                ],
                "DifferentConstBar" => btreeset![
                    "pub_type_alias_of_type_alias::DifferentConstBar",
                ],
                "ReorderedBar" => btreeset![
                    "pub_type_alias_of_type_alias::ReorderedBar",
                ],
                "DefaultValueBar" => btreeset![
                    "pub_type_alias_of_type_alias::DefaultValueBar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_type_alias_of_composite_type() {
            let test_crate = "pub_type_alias_of_composite_type";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "pub_type_alias_of_composite_type::inner::Foo",
                ],
                "I64Tuple" => btreeset![
                    "pub_type_alias_of_composite_type::I64Tuple",
                ],
                "MixedTuple" => btreeset![
                    "pub_type_alias_of_composite_type::MixedTuple",
                ],
                "GenericTuple" => btreeset![
                    "pub_type_alias_of_composite_type::GenericTuple",
                ],
                "LifetimeTuple" => btreeset![
                    "pub_type_alias_of_composite_type::LifetimeTuple",
                ],
                "ConstTuple" => btreeset![
                    "pub_type_alias_of_composite_type::ConstTuple",
                ],
                "DefaultGenericTuple" => btreeset![
                    "pub_type_alias_of_composite_type::DefaultGenericTuple",
                ],
                "DefaultConstTuple" => btreeset![
                    "pub_type_alias_of_composite_type::DefaultConstTuple",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn pub_generic_type_alias_omitted_default() {
            let test_crate = "pub_generic_type_alias_omitted_default";
            let expected_items = btreemap! {
                "DefaultConst" => btreeset![
                    "pub_generic_type_alias_omitted_default::inner::DefaultConst",
                ],
                "DefaultType" => btreeset![
                    "pub_generic_type_alias_omitted_default::inner::DefaultType",
                ],
                "ConstOnly" => btreeset![
                    "pub_generic_type_alias_omitted_default::inner::ConstOnly",
                ],
                "TypeOnly" => btreeset![
                    "pub_generic_type_alias_omitted_default::inner::TypeOnly",
                ],
                "OmittedConst" => btreeset![
                    "pub_generic_type_alias_omitted_default::OmittedConst",
                ],
                "OmittedType" => btreeset![
                    "pub_generic_type_alias_omitted_default::OmittedType",
                ],
                "NonGenericConst" => btreeset![
                    "pub_generic_type_alias_omitted_default::NonGenericConst",
                ],
                "NonGenericType" => btreeset![
                    "pub_generic_type_alias_omitted_default::NonGenericType",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn swapping_names() {
            let test_crate = "swapping_names";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "swapping_names::Foo",
                    "swapping_names::inner::Bar",
                    "swapping_names::inner::nested::Foo",
                ],
                "Bar" => btreeset![
                    "swapping_names::Bar",
                    "swapping_names::inner::Foo",
                    "swapping_names::inner::nested::Bar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn overlapping_glob_and_local_module() {
            let test_crate = "overlapping_glob_and_local_module";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "overlapping_glob_and_local_module::sibling::duplicated::Foo",
                ],
                "Bar" => btreeset![
                    "overlapping_glob_and_local_module::inner::duplicated::Bar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn overlapping_glob_and_renamed_module() {
            let test_crate = "overlapping_glob_and_renamed_module";
            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "overlapping_glob_and_renamed_module::sibling::duplicated::Foo",
                ],
                "Bar" => btreeset![
                    "overlapping_glob_and_renamed_module::inner::duplicated::Bar",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn type_and_value_with_matching_names() {
            let test_crate = "type_and_value_with_matching_names";
            let expected_items = btreemap! {
                "Foo" => (2, btreeset![
                    "type_and_value_with_matching_names::Foo",
                    "type_and_value_with_matching_names::nested::Foo",
                ]),
                "Bar" => (2, btreeset![
                    "type_and_value_with_matching_names::Bar",
                    "type_and_value_with_matching_names::nested::Bar",
                ]),
            };

            assert_duplicated_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn no_shadowing_across_namespaces() {
            let test_crate = "no_shadowing_across_namespaces";
            let expected_items = btreemap! {
                "Foo" => (2, btreeset![
                    "no_shadowing_across_namespaces::Foo",
                    "no_shadowing_across_namespaces::nested::Foo",
                ]),
            };

            assert_duplicated_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn explicit_reexport_of_matching_names() {
            if version_check::is_min_version("1.69.0").unwrap_or(true) {
                let test_crate = "explicit_reexport_of_matching_names";
                let expected_items = btreemap! {
                    "Foo" => (2, btreeset![
                        "explicit_reexport_of_matching_names::Bar",
                        "explicit_reexport_of_matching_names::Foo",
                        "explicit_reexport_of_matching_names::nested::Foo",
                    ]),
                };

                assert_duplicated_exported_items_match(test_crate, &expected_items);
            } else {
                use std::io::Write;
                writeln!(
                    std::io::stderr(),
                    "skipping 'explicit_reexport_of_matching_names' test due to Rust {:?}",
                    version_check::Version::read(),
                )
                .expect("write failed");
            }
        }

        #[test]
        fn overlapping_glob_and_local_item() {
            let test_crate = "overlapping_glob_and_local_item";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let foo_ids = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if foo_ids.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    Foo, but found these matching IDs: {foo_ids:?}"
                );
            }

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| {
                    (matches!(item.name.as_deref(), Some("Foo" | "Bar"))).then_some(id)
                })
                .collect_vec();
            if item_id_candidates.len() != 3 {
                panic!(
                    "Expected to find exactly 3 items named Foo or Bar, \
                    but found these matching IDs: {item_id_candidates:?}"
                );
            }

            let mut all_importable_paths = Vec::new();
            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();
                let deduplicated_actual_items: BTreeSet<_> =
                    actual_items.iter().map(|x| x.as_str()).collect();
                assert_eq!(
                    actual_items.len(),
                    deduplicated_actual_items.len(),
                    "duplicates found: {actual_items:?}"
                );

                if deduplicated_actual_items
                    .first()
                    .expect("no names")
                    .ends_with("::Foo")
                {
                    assert_eq!(
                        deduplicated_actual_items.len(),
                        1,
                        "\
expected exactly one importable path for `Foo` items in this crate but got: {actual_items:?}"
                    );
                } else {
                    assert_eq!(
                        deduplicated_actual_items,
                        btreeset! {
                            "overlapping_glob_and_local_item::Bar",
                            "overlapping_glob_and_local_item::inner::Bar",
                        }
                    );
                }

                all_importable_paths.extend(actual_items);
            }

            all_importable_paths.sort_unstable();
            assert_eq!(
                vec![
                    "overlapping_glob_and_local_item::Bar",
                    "overlapping_glob_and_local_item::Foo",
                    "overlapping_glob_and_local_item::inner::Bar",
                    "overlapping_glob_and_local_item::inner::Foo",
                ],
                all_importable_paths,
            );
        }

        #[test]
        fn nested_overlapping_glob_and_local_item() {
            let test_crate = "nested_overlapping_glob_and_local_item";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    Foo, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            let mut all_importable_paths = Vec::new();
            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();
                let deduplicated_actual_items: BTreeSet<_> =
                    actual_items.iter().map(|x| x.as_str()).collect();

                assert_eq!(
                    actual_items.len(),
                    deduplicated_actual_items.len(),
                    "duplicates found: {actual_items:?}"
                );

                match deduplicated_actual_items.len() {
                    1 => assert_eq!(
                        deduplicated_actual_items,
                        btreeset! { "nested_overlapping_glob_and_local_item::Foo" },
                    ),
                    2 => assert_eq!(
                        deduplicated_actual_items,
                        btreeset! {
                            "nested_overlapping_glob_and_local_item::inner::Foo",
                            "nested_overlapping_glob_and_local_item::inner::nested::Foo",
                        }
                    ),
                    _ => unreachable!("unexpected value for {deduplicated_actual_items:?}"),
                };

                all_importable_paths.extend(actual_items);
            }

            all_importable_paths.sort_unstable();
            assert_eq!(
                vec![
                    "nested_overlapping_glob_and_local_item::Foo",
                    "nested_overlapping_glob_and_local_item::inner::Foo",
                    "nested_overlapping_glob_and_local_item::inner::nested::Foo",
                ],
                all_importable_paths,
            );
        }

        #[test]
        fn cyclic_overlapping_glob_and_local_item() {
            let test_crate = "cyclic_overlapping_glob_and_local_item";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    Foo, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            let mut all_importable_paths = Vec::new();
            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();
                let deduplicated_actual_items: BTreeSet<_> =
                    actual_items.iter().map(|x| x.as_str()).collect();

                assert_eq!(
                    actual_items.len(),
                    deduplicated_actual_items.len(),
                    "duplicates found: {actual_items:?}"
                );

                match deduplicated_actual_items.len() {
                    1 => assert_eq!(
                        btreeset! { "cyclic_overlapping_glob_and_local_item::Foo" },
                        deduplicated_actual_items,
                    ),
                    4 => assert_eq!(
                        btreeset! {
                            "cyclic_overlapping_glob_and_local_item::inner::Foo",
                            "cyclic_overlapping_glob_and_local_item::inner::nested::Foo",
                            "cyclic_overlapping_glob_and_local_item::nested::Foo",
                            "cyclic_overlapping_glob_and_local_item::nested::inner::Foo",
                        },
                        deduplicated_actual_items,
                    ),
                    _ => unreachable!("unexpected value for {deduplicated_actual_items:?}"),
                };

                all_importable_paths.extend(actual_items);
            }

            all_importable_paths.sort_unstable();
            assert_eq!(
                vec![
                    "cyclic_overlapping_glob_and_local_item::Foo",
                    "cyclic_overlapping_glob_and_local_item::inner::Foo",
                    "cyclic_overlapping_glob_and_local_item::inner::nested::Foo",
                    "cyclic_overlapping_glob_and_local_item::nested::Foo",
                    "cyclic_overlapping_glob_and_local_item::nested::inner::Foo",
                ],
                all_importable_paths,
            );
        }

        #[test]
        fn overlapping_glob_of_enum_with_local_item() {
            let test_crate = "overlapping_glob_of_enum_with_local_item";
            let easy_expected_items = btreemap! {
                "Foo" => btreeset![
                    "overlapping_glob_of_enum_with_local_item::Foo",
                ],
                "Second" => btreeset![
                    "overlapping_glob_of_enum_with_local_item::Foo::Second",
                    "overlapping_glob_of_enum_with_local_item::inner::Second",
                ],
            };

            // Check the "easy" cases: `Foo` and `Second`.
            // This is necessary but not sufficient to confirm our implementation works,
            // since it doesn't check anything about `First` which is the point of this test case.
            assert_exported_items_match(test_crate, &easy_expected_items);

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let items_named_first: Vec<_> = indexed_crate
                .inner
                .index
                .values()
                .filter(|item| item.name.as_deref() == Some("First"))
                .collect();
            assert_eq!(2, items_named_first.len(), "{items_named_first:?}");
            let variant_item = items_named_first
                .iter()
                .copied()
                .find(|item| matches!(item.inner, ItemEnum::Variant(..)))
                .expect("no variant item found");
            let struct_item = items_named_first
                .iter()
                .copied()
                .find(|item| matches!(item.inner, ItemEnum::Struct(..)))
                .expect("no struct item found");

            assert_eq!(
                vec![ImportablePath::new(
                    vec!["overlapping_glob_of_enum_with_local_item", "Foo", "First"],
                    false,
                    false,
                )],
                indexed_crate.publicly_importable_names(&variant_item.id),
            );
            assert_eq!(
                // The struct definition overrides the glob-imported variant here.
                vec![ImportablePath::new(
                    vec!["overlapping_glob_of_enum_with_local_item", "inner", "First"],
                    false,
                    false,
                )],
                indexed_crate.publicly_importable_names(&struct_item.id),
            );
        }

        #[test]
        fn glob_of_enum_does_not_shadow_local_fn() {
            let test_crate = "glob_of_enum_does_not_shadow_local_fn";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let first_ids = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("First")).then_some(id))
                .collect_vec();
            if first_ids.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    First, but found these matching IDs: {first_ids:?}"
                );
            }

            for item_id in first_ids {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();
                let deduplicated_actual_items: BTreeSet<_> =
                    actual_items.iter().map(|x| x.as_str()).collect();
                assert_eq!(
                    actual_items.len(),
                    deduplicated_actual_items.len(),
                    "duplicates found: {actual_items:?}"
                );

                let expected_items = match &rustdoc.index[item_id].inner {
                    ItemEnum::Variant(..) => {
                        vec!["glob_of_enum_does_not_shadow_local_fn::Foo::First"]
                    }
                    ItemEnum::Function(..) => {
                        vec!["glob_of_enum_does_not_shadow_local_fn::inner::First"]
                    }
                    other => {
                        unreachable!("item {item_id:?} had unexpected inner content: {other:?}")
                    }
                };

                assert_eq!(expected_items, actual_items);
            }
        }

        /// There's currently no way to detect private imports that shadow glob items.
        /// Reported as: <https://github.com/rust-lang/rust/issues/111338>
        #[test]
        #[should_panic = "expected no importable item names but found \
                         [\"overlapping_glob_and_private_import::inner::Foo\"]"]
        fn overlapping_glob_and_private_import() {
            let test_crate = "overlapping_glob_and_private_import";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    Foo, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();

                assert!(
                    actual_items.is_empty(),
                    "expected no importable item names but found {actual_items:?}"
                );
            }
        }

        /// Our logic for determining whether a tuple struct's implicit constructor is exported
        /// is too simplistic: it assumes "yes" if all fields are pub, and "no" otherwise.
        /// This is why this test currently fails.
        /// TODO: fix this once rustdoc includes shadowing information
        ///       <https://github.com/rust-lang/rust/issues/111338>
        ///
        /// Its sibling test `visibility_modifier_avoids_shadowing` ensures that shadowing is
        /// not inappropriately applied when the tuple constructors do *not* shadow each other.
        #[test]
        #[should_panic = "expected no importable item names but found \
                         [\"visibility_modifier_causes_shadowing::Foo\"]"]
        fn visibility_modifier_causes_shadowing() {
            let test_crate = "visibility_modifier_causes_shadowing";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 3 {
                panic!(
                    "Expected to find exactly 3 items with name \
                    Foo, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();

                assert!(
                    actual_items.is_empty(),
                    "expected no importable item names but found {actual_items:?}"
                );
            }
        }

        #[test]
        fn visibility_modifier_avoids_shadowing() {
            let test_crate = "visibility_modifier_avoids_shadowing";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Foo")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 3 {
                panic!(
                    "Expected to find exactly 3 items with name \
                    Foo, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            for item_id in item_id_candidates {
                let actual_items: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();

                if rustdoc.index[item_id].visibility == Visibility::Public {
                    assert_eq!(
                        vec!["visibility_modifier_avoids_shadowing::Foo"],
                        actual_items,
                    );
                } else {
                    assert!(
                        actual_items.is_empty(),
                        "expected no importable item names but found {actual_items:?}"
                    );
                }
            }
        }

        #[test]
        fn glob_vs_glob_shadowing() {
            let test_crate = "glob_vs_glob_shadowing";

            let expected_items = btreemap! {
                "Foo" => (2, btreeset![]),
                "Bar" => (1, btreeset![
                    "glob_vs_glob_shadowing::Bar",
                ]),
                "Baz" => (1, btreeset![
                    "glob_vs_glob_shadowing::Baz",
                ]),
            };

            assert_duplicated_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_vs_glob_shadowing_downstream() {
            let test_crate = "glob_vs_glob_shadowing_downstream";

            let expected_items = btreemap! {
                "Foo" => (3, btreeset![]),
                "Bar" => (1, btreeset![
                    "glob_vs_glob_shadowing_downstream::second::Bar",
                ]),
            };

            assert_duplicated_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_vs_glob_no_shadowing_for_same_item() {
            let test_crate = "glob_vs_glob_no_shadowing_for_same_item";

            let expected_items = btreemap! {
                "Foo" => btreeset![
                    "glob_vs_glob_no_shadowing_for_same_item::Foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_vs_glob_no_shadowing_for_same_renamed_item() {
            let test_crate = "glob_vs_glob_no_shadowing_for_same_renamed_item";

            let expected_items = btreemap! {
                "Bar" => btreeset![
                    "glob_vs_glob_no_shadowing_for_same_renamed_item::Foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn glob_vs_glob_no_shadowing_for_same_multiply_renamed_item() {
            let test_crate = "glob_vs_glob_no_shadowing_for_same_multiply_renamed_item";

            let expected_items = btreemap! {
                "Bar" => btreeset![
                    "glob_vs_glob_no_shadowing_for_same_multiply_renamed_item::Foo",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn reexport_consts_and_statics() {
            let test_crate = "reexport_consts_and_statics";
            let expected_items = btreemap! {
                "FIRST" => btreeset![
                    "reexport_consts_and_statics::FIRST",
                    "reexport_consts_and_statics::inner::FIRST",
                ],
                "SECOND" => btreeset![
                    "reexport_consts_and_statics::SECOND",
                    "reexport_consts_and_statics::inner::SECOND",
                ],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn reexport_as_underscore() {
            let test_crate = "reexport_as_underscore";
            let expected_items = btreemap! {
                "Struct" => btreeset![
                    "reexport_as_underscore::Struct",
                ],
                "Trait" => btreeset![],
                "hidden" => btreeset![],
                "UnderscoreImported" => btreeset![],
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn nested_reexport_as_underscore() {
            let test_crate = "nested_reexport_as_underscore";
            let expected_items = btreemap! {
                "Trait" => btreeset![],  // no importable paths!
            };

            assert_exported_items_match(test_crate, &expected_items);
        }

        #[test]
        fn overlapping_reexport_as_underscore() {
            let test_crate = "overlapping_reexport_as_underscore";

            let rustdoc = load_pregenerated_rustdoc(test_crate);
            let indexed_crate = IndexedCrate::new(&rustdoc);

            let item_id_candidates = rustdoc
                .index
                .iter()
                .filter_map(|(id, item)| (item.name.as_deref() == Some("Example")).then_some(id))
                .collect_vec();
            if item_id_candidates.len() != 2 {
                panic!(
                    "Expected to find exactly 2 items with name \
                    Example, but found these matching IDs: {item_id_candidates:?}"
                );
            }

            for item_id in item_id_candidates {
                let importable_paths: Vec<_> = indexed_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(|importable| importable.path.components.into_iter().join("::"))
                    .collect();

                match &rustdoc.index[item_id].inner {
                    ItemEnum::Struct(..) => {
                        assert_eq!(
                            vec!["overlapping_reexport_as_underscore::Example"],
                            importable_paths,
                        );
                    }
                    ItemEnum::Trait(..) => {
                        assert!(
                            importable_paths.is_empty(),
                            "expected no importable item names but found {importable_paths:?}"
                        );
                    }
                    _ => unreachable!(
                        "unexpected item for ID {item_id:?}: {:?}",
                        rustdoc.index[item_id]
                    ),
                }
            }
        }
    }
}
