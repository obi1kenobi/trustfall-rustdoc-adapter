use std::collections::HashMap;

use rustdoc_types::{Crate, Id, Item, Visibility};

/// The rustdoc for a crate, together with associated indexed data to speed up common operations.
///
/// Besides the parsed rustdoc, it also contains some manually-inlined `rustdoc_types::Trait`s
/// of the most common built-in traits.
/// This is a temporary step, until we're able to combine rustdocs of multiple crates.
#[derive(Debug, Clone)]
pub struct IndexedCrate<'a> {
    pub(crate) inner: &'a Crate,

    // For an Id, give the list of item Ids under which it is publicly visible.
    pub(crate) visibility_forest: HashMap<&'a Id, Vec<&'a Id>>,

    /// Trait items defined in external crates are not present in the `inner: &Crate` field,
    /// even if they are implemented by a type in that crate. This also includes
    /// Rust's built-in traits like `Debug, Send, Eq` etc.
    ///
    /// This change is approximately as of rustdoc v23,
    /// in https://github.com/rust-lang/rust/pull/105182
    ///
    /// As a temporary workaround, we manually create the trait items
    /// for the most common Rust built-in traits and link to those items
    /// as if they were still part of the rustdoc JSON file.
    ///
    /// A more complete future solution may generate multiple crates' rustdoc JSON
    /// and link to the external crate's trait items as necessary.
    pub(crate) manually_inlined_builtin_traits: HashMap<Id, Item>,
}

impl<'a> IndexedCrate<'a> {
    pub fn new(crate_: &'a Crate) -> Self {
        Self {
            inner: crate_,
            visibility_forest: calculate_visibility_forest(crate_),
            manually_inlined_builtin_traits: create_manually_inlined_builtin_traits(crate_),
        }
    }

    pub fn publicly_importable_names(&self, id: &'a Id) -> Vec<Vec<&'a str>> {
        let mut result = vec![];

        if self.inner.index.contains_key(id) {
            self.collect_publicly_importable_names(id, &mut vec![], &mut result);
        }

        result
    }

    fn collect_publicly_importable_names(
        &self,
        next_id: &'a Id,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        let item = &self.inner.index[next_id];
        if let Some(item_name) = item.name.as_deref() {
            stack.push(item_name);
        } else {
            assert!(
                matches!(item.inner, rustdoc_types::ItemEnum::Import(..)),
                "{item:?}"
            );
        }

        if next_id == &self.inner.root {
            let final_name = stack.iter().rev().copied().collect();
            output.push(final_name);
        } else if let Some(visible_parents) = self.visibility_forest.get(next_id) {
            for parent_id in visible_parents.iter().copied() {
                self.collect_publicly_importable_names(parent_id, stack, output);
            }
        }

        if let Some(item_name) = item.name.as_deref() {
            let popped_item = stack.pop().expect("stack was unexpectedly empty");
            assert_eq!(item_name, popped_item);
        }
    }
}

fn calculate_visibility_forest(crate_: &Crate) -> HashMap<&Id, Vec<&Id>> {
    let mut result = Default::default();
    let root_id = &crate_.root;
    if let Some(root_module) = crate_.index.get(root_id) {
        if root_module.visibility == Visibility::Public {
            collect_public_items(crate_, &mut result, root_module, None);
        }
    }

    result
}

fn collect_public_items<'a>(
    crate_: &'a Crate,
    pub_items: &mut HashMap<&'a Id, Vec<&'a Id>>,
    item: &'a Item,
    parent_id: Option<&'a Id>,
) {
    match item.visibility {
        // Some impls and methods have default visibility:
        // they are visible only if the type to which they belong is visible.
        // However, we don't recurse into non-public items with this function, so
        // reachable items with default visibility must be public.
        Visibility::Public | Visibility::Default => {
            let parents = pub_items.entry(&item.id).or_default();
            if let Some(parent_id) = parent_id {
                parents.push(parent_id);
            }

            let next_parent_id = Some(&item.id);
            match &item.inner {
                rustdoc_types::ItemEnum::Module(m) => {
                    for inner in m.items.iter().filter_map(|id| crate_.index.get(id)) {
                        collect_public_items(crate_, pub_items, inner, next_parent_id);
                    }
                }
                rustdoc_types::ItemEnum::Import(imp) => {
                    // TODO: handle glob imports (`pub use foo::bar::*`) here.
                    if let Some(item) = imp.id.as_ref().and_then(|id| crate_.index.get(id)) {
                        collect_public_items(crate_, pub_items, item, next_parent_id);
                    }
                }
                rustdoc_types::ItemEnum::Struct(struct_) => {
                    let field_ids_iter: Box<dyn Iterator<Item = &Id>> = match &struct_.kind {
                        rustdoc_types::StructKind::Unit => Box::new(std::iter::empty()),
                        rustdoc_types::StructKind::Tuple(field_ids) => {
                            Box::new(field_ids.iter().filter_map(|x| x.as_ref()))
                        }
                        rustdoc_types::StructKind::Plain { fields, .. } => Box::new(fields.iter()),
                    };

                    for inner in field_ids_iter
                        .chain(struct_.impls.iter())
                        .filter_map(|id| crate_.index.get(id))
                    {
                        collect_public_items(crate_, pub_items, inner, next_parent_id);
                    }
                }
                rustdoc_types::ItemEnum::Enum(enum_) => {
                    for inner in enum_
                        .variants
                        .iter()
                        .chain(enum_.impls.iter())
                        .filter_map(|id| crate_.index.get(id))
                    {
                        collect_public_items(crate_, pub_items, inner, next_parent_id);
                    }
                }
                rustdoc_types::ItemEnum::Trait(trait_) => {
                    for inner in trait_.items.iter().filter_map(|id| crate_.index.get(id)) {
                        collect_public_items(crate_, pub_items, inner, next_parent_id);
                    }
                }
                rustdoc_types::ItemEnum::Impl(impl_) => {
                    for inner in impl_.items.iter().filter_map(|id| crate_.index.get(id)) {
                        collect_public_items(crate_, pub_items, inner, next_parent_id);
                    }
                }
                _ => {
                    // No-op, no further items within to consider.
                }
            }
        }
        Visibility::Crate | Visibility::Restricted { .. } => {}
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
