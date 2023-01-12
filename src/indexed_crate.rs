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
            visibility_forest: compute_parent_ids_for_public_items(crate_),
            manually_inlined_builtin_traits: create_manually_inlined_builtin_traits(crate_),
        }
    }

    /// Return all the paths (as Vec<&'a str> of component names, joinable with "::")
    /// with which the given item can be imported from this crate.
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

        let (push_name, popped_name) = match &item.inner {
            rustdoc_types::ItemEnum::Import(import_item) => {
                if import_item.glob {
                    // Glob imports refer to the *contents* of the named item, not the item itself.
                    // Rust doesn't allow glob imports to rename items, so there's no name to add.
                    (None, None)
                } else {
                    // Use the name of the imported item, since it might be renaming
                    // the item being imported.
                    let push_name = Some(import_item.name.as_str());

                    // The imported item may be renamed here, so pop it from the stack.
                    //
                    // Due to how we record Id -> parent Id relationships,
                    // if the imported (and perhaps renamed) item is a module,
                    // its name was never added to the stack so we don't have to pop it here.
                    let popped_name = {
                        let should_pop = import_item
                            .id
                            .as_ref()
                            .and_then(|id| self.inner.index.get(id))
                            .map(|item| !matches!(&item.inner, rustdoc_types::ItemEnum::Module(..)))
                            .unwrap_or_default();
                        if should_pop {
                            Some(stack.pop().expect("no name to pop"))
                        } else {
                            None
                        }
                    };

                    (push_name, popped_name)
                }
            }
            _ => (item.name.as_deref(), None),
        };

        // Push the new name onto the stack, if there is one.
        if let Some(pushed_name) = push_name {
            stack.push(pushed_name);
        }

        self.collect_publicly_importable_names_inner(next_id, stack, output);

        // Undo any changes made to the stack, returning it to its pre-recursion state.
        if let Some(pushed_name) = push_name {
            let recovered_name = stack.pop().expect("there was nothing to pop");
            assert_eq!(pushed_name, recovered_name);
        }
        if let Some(popped_name) = popped_name {
            stack.push(popped_name);
        }
    }

    fn collect_publicly_importable_names_inner(
        &self,
        next_id: &'a Id,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        if next_id == &self.inner.root {
            let final_name = stack.iter().rev().copied().collect();
            output.push(final_name);
        } else if let Some(visible_parents) = self.visibility_forest.get(next_id) {
            for parent_id in visible_parents.iter().copied() {
                self.collect_publicly_importable_names(parent_id, stack, output);
            }
        }
    }
}

fn compute_parent_ids_for_public_items(crate_: &Crate) -> HashMap<&Id, Vec<&Id>> {
    let mut result = Default::default();
    let root_id = &crate_.root;
    if let Some(root_module) = crate_.index.get(root_id) {
        if root_module.visibility == Visibility::Public {
            visit_root_reachable_public_items(crate_, &mut result, root_module, None);
        }
    }

    result
}

/// Collect all public items that are reachable from the crate root and record their parent Ids.
fn visit_root_reachable_public_items<'a>(
    crate_: &'a Crate,
    parents: &mut HashMap<&'a Id, Vec<&'a Id>>,
    item: &'a Item,
    parent_id: Option<&'a Id>,
) {
    match item.visibility {
        Visibility::Crate | Visibility::Restricted { .. } => {
            // This item is not public, so we don't need to process it.
            return;
        }
        Visibility::Public => {} // Public item, keep going.
        Visibility::Default => {
            // Enum variants, and some impls and methods have default visibility:
            // they are visible only if the type to which they belong is visible.
            // However, we don't recurse into non-public items with this function, so
            // reachable items with default visibility must be public.
        }
    }

    let item_parents = parents.entry(&item.id).or_default();
    if let Some(parent_id) = parent_id {
        item_parents.push(parent_id);
    }

    let next_parent_id = Some(&item.id);
    match &item.inner {
        rustdoc_types::ItemEnum::Module(m) => {
            for inner in m.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(crate_, parents, inner, next_parent_id);
            }
        }
        rustdoc_types::ItemEnum::Import(imp) => {
            // Imports of modules, and glob imports of enums,
            // import the *contents* of the pointed-to item rather than the item itself.
            if let Some(imported_item) = imp.id.as_ref().and_then(|id| crate_.index.get(id)) {
                if let rustdoc_types::ItemEnum::Module(module_item) = &imported_item.inner {
                    for inner_id in &module_item.items {
                        if let Some(item) = crate_.index.get(inner_id) {
                            visit_root_reachable_public_items(
                                crate_,
                                parents,
                                item,
                                next_parent_id,
                            );
                        }
                    }
                } else if imp.glob {
                    if let rustdoc_types::ItemEnum::Enum(enum_item) = &imported_item.inner {
                        for inner_id in &enum_item.variants {
                            if let Some(item) = crate_.index.get(inner_id) {
                                visit_root_reachable_public_items(
                                    crate_,
                                    parents,
                                    item,
                                    next_parent_id,
                                );
                            }
                        }
                    } else {
                        unreachable!("found a glob import of an unexpected kind of item: {imp:?} {imported_item:?}")
                    }
                } else {
                    visit_root_reachable_public_items(
                        crate_,
                        parents,
                        imported_item,
                        next_parent_id,
                    );
                }
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
                visit_root_reachable_public_items(crate_, parents, inner, next_parent_id);
            }
        }
        rustdoc_types::ItemEnum::Enum(enum_) => {
            for inner in enum_
                .variants
                .iter()
                .chain(enum_.impls.iter())
                .filter_map(|id| crate_.index.get(id))
            {
                visit_root_reachable_public_items(crate_, parents, inner, next_parent_id);
            }
        }
        rustdoc_types::ItemEnum::Trait(trait_) => {
            for inner in trait_.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(crate_, parents, inner, next_parent_id);
            }
        }
        rustdoc_types::ItemEnum::Impl(impl_) => {
            for inner in impl_.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(crate_, parents, inner, next_parent_id);
            }
        }
        _ => {
            // No-op, no further items within to consider.
        }
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

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use anyhow::Context;
    use rustdoc_types::Crate;

    fn load_pregenerated_rustdoc(crate_name: &str) -> Crate {
        let path = format!("./localdata/test_data/{crate_name}/rustdoc.json");
        let content = read_to_string(&path)
            .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to load rustdoc");
        serde_json::from_str(&content)
            .with_context(|| format!("Failed to parse {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to parse rustdoc JSON")
    }

    mod reexports {
        use std::collections::{BTreeMap, BTreeSet};

        use itertools::Itertools;
        use maplit::{btreemap, btreeset};

        use crate::IndexedCrate;

        use super::load_pregenerated_rustdoc;

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
                    .map(|components| components.into_iter().join("::"))
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
    }
}
