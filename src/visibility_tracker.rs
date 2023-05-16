use std::collections::{HashMap, HashSet};

use rustdoc_types::{Crate, GenericArgs, Id, Item, ItemEnum, Typedef, Visibility};

#[derive(Debug, Clone)]
pub(crate) struct VisibilityTracker<'a> {
    // The crate this represents.
    inner: &'a Crate,

    /// For an Id, give the list of item Ids under which it is publicly visible.
    visible_parent_ids: HashMap<&'a Id, Vec<&'a Id>>,
}

impl<'a> VisibilityTracker<'a> {
    pub(crate) fn from_crate(crate_: &'a Crate) -> Self {
        let visible_parent_ids = compute_parent_ids_for_public_items(crate_)
            .into_iter()
            .map(|(key, values)| {
                // Ensure a consistent order, since queries can observe this order directly.
                let mut values: Vec<_> = values.into_iter().collect();
                values.sort_unstable_by_key(|x| &x.0);
                (key, values)
            })
            .collect();

        Self {
            inner: crate_,
            visible_parent_ids,
        }
    }

    pub(crate) fn collect_publicly_importable_names(
        &self,
        next_id: &'a Id,
        already_visited_ids: &mut HashSet<&'a Id>,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        if !already_visited_ids.insert(next_id) {
            // We found a cycle, and we've already processed this item.
            // Nothing more to do here.
            return;
        }

        let item = &self.inner.index[next_id];
        if !stack.is_empty()
            && matches!(
                item.inner,
                ItemEnum::Impl(..) | ItemEnum::Struct(..) | ItemEnum::Union(..)
            )
        {
            // Structs, unions, and impl blocks are not modules.
            // They *themselves* can be imported, but the items they contain cannot be imported.
            // Since the stack is non-empty, we must be trying to determine importable names
            // for a descendant item of a struct / union / impl. There are none.
            //
            // We explicitly do *not* want to check for Enum here,
            // since enum variants *are* importable.
            return;
        }

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
                    let popped_name = Some(stack.pop().expect("no name to pop"));

                    (push_name, popped_name)
                }
            }
            rustdoc_types::ItemEnum::Typedef(..) => {
                // Use the typedef name instead of the underlying item's own name,
                // since it might be renaming the underlying item.
                let push_name = Some(item.name.as_deref().expect("typedef had no name"));

                // If there is an underlying item, pop it from the stack
                // since it may be renamed here.
                let popped_name = stack.pop();

                (push_name, popped_name)
            }
            _ => (item.name.as_deref(), None),
        };

        // Push the new name onto the stack, if there is one.
        if let Some(pushed_name) = push_name {
            stack.push(pushed_name);
        }

        self.collect_publicly_importable_names_inner(next_id, already_visited_ids, stack, output);

        // Undo any changes made to the stack, returning it to its pre-recursion state.
        if let Some(pushed_name) = push_name {
            let recovered_name = stack.pop().expect("there was nothing to pop");
            assert_eq!(pushed_name, recovered_name);
        }
        if let Some(popped_name) = popped_name {
            stack.push(popped_name);
        }

        // We're leaving this item. Remove it from the visited set.
        let removed = already_visited_ids.remove(next_id);
        assert!(removed);
    }

    fn collect_publicly_importable_names_inner(
        &self,
        next_id: &'a Id,
        already_visited_ids: &mut HashSet<&'a Id>,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        if next_id == &self.inner.root {
            let final_name = stack.iter().rev().copied().collect();
            output.push(final_name);
        } else if let Some(visible_parents) = self.visible_parent_ids.get(next_id) {
            for parent_id in visible_parents.iter().copied() {
                self.collect_publicly_importable_names(
                    parent_id,
                    already_visited_ids,
                    stack,
                    output,
                );
            }
        }
    }

    #[cfg(test)]
    pub(super) fn visible_parent_ids(&self) -> &HashMap<&'a Id, Vec<&'a Id>> {
        &self.visible_parent_ids
    }
}

/// A Rust item name, together with the namespace the name is in.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NamespacedName<'a> {
    Values(&'a str),
    Types(&'a str),
}

impl<'a> NamespacedName<'a> {
    fn rename(&self, new_name: &'a str) -> Self {
        match self {
            NamespacedName::Values(_) => NamespacedName::Values(new_name),
            NamespacedName::Types(_) => NamespacedName::Types(new_name),
        }
    }
}

#[derive(Debug, Clone)]
struct Definition<'a> {
    /// The Id of this definition.
    ///
    /// When the definition is an import, the `current_id` is the Id of the import,
    /// to account for possible renamings.
    /// Otherwise, the `current_id` should be the same as the `underlying_id`.
    current_id: &'a Id,

    /// The actual underlying item this definition resolves to, like a struct or function.
    /// This Id must not point to an import item.
    final_underlying_id: &'a Id,
}

impl<'a> Definition<'a> {
    fn new(current_id: &'a Id, final_underlying_id: &'a Id) -> Self {
        Self {
            current_id,
            final_underlying_id,
        }
    }

    fn new_direct(id: &'a Id) -> Self {
        Self {
            current_id: id,
            final_underlying_id: id,
        }
    }
}

/// Type showing which names are defined in which modules, where they point to,
/// and whether they are defined directly, or imported directly or via a glob.
#[derive(Debug, Default)]
struct NameResolution<'a> {
    /// Module Id -> { name -> (id, is_public) } for items directly defined in that module.
    /// Not just public names, since private names can shadow pub glob-exported names.
    names_defined_in_module: HashMap<&'a Id, HashMap<NamespacedName<'a>, (Definition<'a>, bool)>>,

    /// Modules and the glob imports they contain.
    modules_with_glob_imports: HashMap<&'a Id, HashSet<&'a Id>>,

    /// Names that were glob-imported and re-exported into a module, together with
    /// the item Id to which they refer. This is because glob-glob name shadowing doesn't apply
    /// if both names point to the same item.
    glob_imported_names_in_module: HashMap<&'a Id, HashMap<NamespacedName<'a>, Definition<'a>>>,

    /// Names in a module that were glob-imported more than once, and are therefore unusable.
    duplicated_glob_names_in_module: HashMap<&'a Id, HashSet<NamespacedName<'a>>>,
}

fn compute_parent_ids_for_public_items(crate_: &Crate) -> HashMap<&Id, HashSet<&Id>> {
    let mut result = Default::default();
    let root_id = &crate_.root;

    if let Some(root_module) = crate_.index.get(root_id) {
        if root_module.visibility == Visibility::Public {
            let traversal_state = resolve_crate_names(crate_);

            // Avoid cycles by keeping track of which items we're in the middle of visiting.
            let mut currently_visited_items: HashSet<&Id> = Default::default();

            visit_root_reachable_public_items(
                crate_,
                &mut result,
                &traversal_state,
                &mut currently_visited_items,
                root_module,
                None,
            );
        }
    }

    result
}

fn get_names_for_item<'a>(
    crate_: &'a Crate,
    item: &'a Item,
) -> impl Iterator<Item = NamespacedName<'a>> + 'a {
    match &item.inner {
        ItemEnum::Module(..)
        | ItemEnum::Union(..)
        | ItemEnum::Enum(..)
        | ItemEnum::Trait(..)
        | ItemEnum::Typedef(..) => {
            let item_name = item.name.as_deref().expect("item did not have a name");
            [Some(NamespacedName::Types(item_name)), None]
                .into_iter()
                .flatten()
        }
        ItemEnum::Struct(struct_item) => {
            let item_name = item.name.as_deref().expect("item did not have a name");
            match &struct_item.kind {
                rustdoc_types::StructKind::Unit => {
                    // Always both a type and a value (the singleton instance of the type).
                    [
                        Some(NamespacedName::Types(item_name)),
                        Some(NamespacedName::Values(item_name)),
                    ]
                    .into_iter()
                    .flatten()
                }
                rustdoc_types::StructKind::Tuple(tuple_struct) => {
                    // Always a type name, can also be a value if all fields
                    // are visible to the importing scope.
                    // TODO: We only check if the fields are public, which is subtly incorrect.
                    //       We have a test crate for this: `visibility_modifier_causes_shadowing`
                    let nonpublic_field =
                        tuple_struct
                            .iter()
                            .filter_map(|x| x.as_ref())
                            .any(|field_id| {
                                crate_
                                    .index
                                    .get(field_id)
                                    .map(|field| field.visibility != Visibility::Public)
                                    .unwrap_or(false)
                            });
                    if nonpublic_field {
                        [Some(NamespacedName::Types(item_name)), None]
                            .into_iter()
                            .flatten()
                    } else {
                        [
                            Some(NamespacedName::Types(item_name)),
                            Some(NamespacedName::Values(item_name)),
                        ]
                        .into_iter()
                        .flatten()
                    }
                }
                rustdoc_types::StructKind::Plain { .. } => {
                    // Only a type, never a value.
                    [Some(NamespacedName::Types(item_name)), None]
                        .into_iter()
                        .flatten()
                }
            }
        }
        ItemEnum::Variant(..)
        | ItemEnum::Function(..)
        | ItemEnum::Constant(..)
        | ItemEnum::Static(..) => {
            let item_name = item.name.as_deref().expect("item did not have a name");
            [Some(NamespacedName::Values(item_name)), None]
                .into_iter()
                .flatten()
        }
        _ => [None, None].into_iter().flatten(),
    }
}

fn resolve_crate_names(crate_: &Crate) -> NameResolution<'_> {
    let mut result = NameResolution::default();

    for item in crate_.index.values() {
        let ItemEnum::Module(module_item) = &item.inner else { continue; };
        for inner_id in &module_item.items {
            let Some(inner_item) = crate_.index.get(inner_id) else { continue; };

            if let ItemEnum::Import(imp) = &inner_item.inner {
                if imp.glob {
                    result
                        .modules_with_glob_imports
                        .entry(&item.id)
                        .or_default()
                        .insert(inner_id);
                } else if let Some(target) = imp.id.as_ref().and_then(|id| crate_.index.get(id)) {
                    for name in get_names_for_item(crate_, target) {
                        // Handle renaming imports like `use some::foo as bar;`
                        let name = name.rename(&imp.name);

                        // Resolve the final item to which this import points.
                        // This is important to ensure we don't incorrectly decide that
                        // two glob imports shadow each other when they point to the same item.
                        //
                        // TODO: This only handles within-crate imports. It'll need to be updated
                        //       when we support multiple crates and cross-crate imports.
                        let mut underlying_item = target;
                        let final_underlying_id = loop {
                            if let ItemEnum::Import(next_import) = &underlying_item.inner {
                                match next_import.id.as_ref().and_then(|id| crate_.index.get(id)) {
                                    None => break None,
                                    Some(item) => underlying_item = item,
                                }
                            } else {
                                break Some(&underlying_item.id);
                            }
                        };
                        let Some(final_underlying_id) = final_underlying_id else { continue; };
                        let definition = Definition::new(inner_id, final_underlying_id);

                        result
                            .names_defined_in_module
                            .entry(&item.id)
                            .or_default()
                            .insert(
                                name,
                                (
                                    definition,
                                    matches!(
                                        target.visibility,
                                        Visibility::Public | Visibility::Default
                                    ),
                                ),
                            );
                    }
                }
            } else {
                for name in get_names_for_item(crate_, inner_item) {
                    result
                        .names_defined_in_module
                        .entry(&item.id)
                        .or_default()
                        .insert(
                            name,
                            (
                                Definition::new_direct(&inner_item.id),
                                matches!(
                                    inner_item.visibility,
                                    Visibility::Public | Visibility::Default
                                ),
                            ),
                        );
                }
            }
        }
    }

    resolve_glob_imported_names(crate_, &mut result);

    result
}

fn resolve_glob_imported_names<'a>(crate_: &'a Crate, traversal_state: &mut NameResolution<'a>) {
    for (&module_id, globs) in &traversal_state.modules_with_glob_imports {
        let mut visited: HashSet<&Id> = Default::default();
        let mut names = Default::default();
        let mut duplicated_names = Default::default();

        visited.insert(module_id);
        for &glob_id in globs {
            recursively_compute_visited_names_for_glob(
                crate_,
                module_id,
                glob_id,
                &*traversal_state,
                &mut visited,
                &mut names,
                &mut duplicated_names,
            );
        }

        // Glob-of-glob import chains might still produce `names` and `duplicated_names` entries
        // that would be shadowed by locally-defined names in this module. Apply the shadowing
        // rules by removing any conflicing names from both of those collections.
        if let Some(local_names) = traversal_state.names_defined_in_module.get(module_id) {
            for local_name in local_names.keys() {
                names.remove(local_name);
                duplicated_names.remove(local_name);
            }
        }

        if !names.is_empty() {
            traversal_state
                .glob_imported_names_in_module
                .insert(module_id, names);
        }
        if !duplicated_names.is_empty() {
            traversal_state
                .duplicated_glob_names_in_module
                .insert(module_id, duplicated_names);
        }
    }
}

fn recursively_compute_visited_names_for_glob<'a>(
    crate_: &'a Crate,
    glob_parent_module_id: &'a Id,
    glob_id: &'a Id,
    traversal_state: &NameResolution<'a>,
    visited: &mut HashSet<&'a Id>,
    names: &mut HashMap<NamespacedName<'a>, Definition<'a>>,
    duplicated_names: &mut HashSet<NamespacedName<'a>>,
) {
    let ItemEnum::Import(glob_import) = &crate_.index[glob_id].inner else {
        unreachable!("Id {glob_id:?} was not a glob: {:?}", crate_.index[glob_id]);
    };
    assert!(glob_import.glob, "not a glob import: {glob_import:?}");

    let module_local_items = traversal_state
        .names_defined_in_module
        .get(glob_parent_module_id);

    // Glob imports can target both enums and modules. Figure out which one this is.
    let target_id = glob_import
        .id
        .as_ref()
        .expect("no target Id for glob import");

    if let Some(ItemEnum::Enum(enum_item)) = &crate_.index.get(target_id).map(|item| &item.inner) {
        for variant_id in &enum_item.variants {
            if let Some(variant_item) = crate_.index.get(variant_id) {
                let name = NamespacedName::Values(
                    variant_item.name.as_deref().expect("no name for variant"),
                );

                register_name(
                    module_local_items,
                    name,
                    Definition::new_direct(variant_id),
                    names,
                    duplicated_names,
                );
            }
        }
        return;
    }

    let module_id = target_id;
    if !visited.insert(module_id) {
        // Already checked this module.
        return;
    }

    // Process the public locally-defined items.
    if let Some(names_in_module) = traversal_state.names_defined_in_module.get(module_id) {
        for (local_name, data) in names_in_module {
            let (item_defn, is_public) = data;
            if *is_public {
                register_name(
                    module_local_items,
                    *local_name,
                    item_defn.clone(),
                    names,
                    duplicated_names,
                );
            }
        }
    }

    // Recurse into any glob imports defined here.
    if let Some(globs) = traversal_state.modules_with_glob_imports.get(module_id) {
        for &glob_id in globs {
            recursively_compute_visited_names_for_glob(
                crate_,
                module_id,
                glob_id,
                traversal_state,
                visited,
                names,
                duplicated_names,
            );
        }
    }
}

fn register_name<'a>(
    module_local_items: Option<&HashMap<NamespacedName, (Definition<'a>, bool)>>,
    name: NamespacedName<'a>,
    definition: Definition<'a>,
    names: &mut HashMap<NamespacedName<'a>, Definition<'a>>,
    duplicated_names: &mut HashSet<NamespacedName<'a>>,
) {
    // Don't add names that would be shadowed by an explicit definition
    // in the glob's parent module.
    if module_local_items
        .map(|items| !items.contains_key(&name))
        .unwrap_or(true)
    {
        match names.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                if entry.get().final_underlying_id != definition.final_underlying_id {
                    // Duplicate name, remove from here and move to duplicates.
                    entry.remove();
                    duplicated_names.insert(name);
                }
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                if !duplicated_names.contains(&name) {
                    entry.insert(definition);
                }
            }
        }
    }
}

/// Collect all public items that are reachable from the crate root and record their parent Ids.
fn visit_root_reachable_public_items<'a>(
    crate_: &'a Crate,
    parents: &mut HashMap<&'a Id, HashSet<&'a Id>>,
    traversal_state: &NameResolution<'a>,
    currently_visited_items: &mut HashSet<&'a Id>,
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
        item_parents.insert(parent_id);
    }

    if !currently_visited_items.insert(&item.id) {
        // We found a cycle in the import graph, and we've already processed this item.
        // Nothing more to do here.
        return;
    }

    let next_parent_id = Some(&item.id);
    match &item.inner {
        rustdoc_types::ItemEnum::Module(m) => {
            for inner in m.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }

            // Explicitly process items imported via globs inside this module,
            // since the logic there is not item-wise: it requires
            // knowledge of the other names defined in the module.
            if let Some(glob_imports) = traversal_state.glob_imported_names_in_module.get(&item.id)
            {
                for inner_defn in glob_imports.values() {
                    if let Some(inner_item) = crate_.index.get(inner_defn.current_id) {
                        // Glob imports point directly to the contents of the pointed-to module.
                        // For each glob-imported item in this module,
                        // this module is their parent and not the glob import.
                        visit_root_reachable_public_items(
                            crate_,
                            parents,
                            traversal_state,
                            currently_visited_items,
                            inner_item,
                            next_parent_id,
                        );
                    }
                }
            }
        }
        rustdoc_types::ItemEnum::Import(imp) => {
            // Imports of modules, and glob imports of enums,
            // import the *contents* of the pointed-to item rather than the item itself.
            if let Some(imported_item) = imp.id.as_ref().and_then(|id| crate_.index.get(id)) {
                // Glob imports are handled at the level of the module that contains them.
                // Here we just skip them as a no-op.
                if !imp.glob {
                    visit_root_reachable_public_items(
                        crate_,
                        parents,
                        traversal_state,
                        currently_visited_items,
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
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Enum(enum_) => {
            for inner in enum_
                .variants
                .iter()
                .chain(enum_.impls.iter())
                .filter_map(|id| crate_.index.get(id))
            {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Union(union_) => {
            for inner in union_
                .fields
                .iter()
                .chain(union_.impls.iter())
                .filter_map(|id| crate_.index.get(id))
            {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Trait(trait_) => {
            for inner in trait_.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Impl(impl_) => {
            for inner in impl_.items.iter().filter_map(|id| crate_.index.get(id)) {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Typedef(ty) => {
            // We're interested in type aliases that are specifically used to rename types:
            //   `pub type Foo = Bar`
            // If the underlying type is generic, it's only a valid renaming if the typedef
            // is also generic in all the same parameters.
            //
            // The Rust compiler ignores `where` bounds on typedefs, so we ignore them too.
            if let Some(reexport_target) = get_typedef_equivalent_reexport_target(crate_, ty) {
                visit_root_reachable_public_items(
                    crate_,
                    parents,
                    traversal_state,
                    currently_visited_items,
                    reexport_target,
                    next_parent_id,
                );
            }
        }
        _ => {
            // No-op, no further items within to consider.
        }
    }

    // We are leaving this item. Remove it from the visited set.
    let removed = currently_visited_items.remove(&item.id);
    assert!(removed);
}

/// Type aliases can sometimes be equivalent to a regular `pub use` re-export:
/// `pub type Foo = crate::Bar` is an example, equivalent to `pub use crate::Bar`.
///
/// If the underlying type has generic parameters, the type alias must include
/// all the same generic parameters in the same order.
/// `pub type Foo<A, B> = crate::Bar<B, A>` is *not* equivalent to `pub use crate::Bar`.
///
/// If the underlying type has default values for any of its generic parameters,
/// the same exact parameters with the same order and defaults must be present on the type alias.
/// `pub type Foo<A> = crate::Bar<A>` is *not* equivalent to `crate::Bar<A, B = ()>`
/// since `Foo<A, B = i64>` is not valid whereas `crate::Bar<A, B = i64>` is fine.
fn get_typedef_equivalent_reexport_target<'a>(
    crate_: &'a Crate,
    ty: &'a Typedef,
) -> Option<&'a Item> {
    if let rustdoc_types::Type::ResolvedPath(resolved_path) = &ty.type_ {
        let underlying = crate_.index.get(&resolved_path.id)?;

        if let Some(GenericArgs::AngleBracketed { args, bindings }) = resolved_path.args.as_deref()
        {
            if !bindings.is_empty() {
                // The type alias specifies some of the underlying type's generic parameters.
                // This is not equivalent to a re-export.
                return None;
            }

            let underlying_generics = match &underlying.inner {
                rustdoc_types::ItemEnum::Struct(struct_) => &struct_.generics,
                rustdoc_types::ItemEnum::Enum(enum_) => &enum_.generics,
                rustdoc_types::ItemEnum::Trait(trait_) => &trait_.generics,
                rustdoc_types::ItemEnum::Union(union_) => &union_.generics,
                rustdoc_types::ItemEnum::Typedef(ty) => &ty.generics,
                _ => unreachable!("unexpected underlying item kind: {underlying:?}"),
            };

            // For the typedef to be equivalent to a re-export, all of the following must hold:
            // - The typedef has the same number of generic parameters as the underlying.
            // - All underlying generic parameters are available on the typedef,
            //   are of the same kind, in the same order, with the same defaults.
            if ty.generics.params.len() != args.len() {
                // The typedef takes a different number of parameters than
                // it supplies to the underlying type. It cannot be a re-export.
                return None;
            }
            if underlying_generics.params.len() != args.len() {
                // The underlying type supports more generic parameter than the typedef supplies
                // when using it -- the unspecified generic parameters take the default values
                // that must have been specified on the underlying type.
                // Nevertheless, this is not a re-export since the types are not equivalent.
                return None;
            }
            for (ty_generic, (underlying_param, arg_generic)) in ty
                .generics
                .params
                .iter()
                .zip(underlying_generics.params.iter().zip(args.iter()))
            {
                let arg_generic_name = match arg_generic {
                    rustdoc_types::GenericArg::Lifetime(name) => name.as_str(),
                    rustdoc_types::GenericArg::Type(rustdoc_types::Type::Generic(t)) => t.as_str(),
                    rustdoc_types::GenericArg::Type(_) => return None,
                    rustdoc_types::GenericArg::Const(c) => {
                        // Nominally, this is the const expression, not the const generic's name.
                        // However, except for pathological edge cases, if the expression is not
                        // simply the const generic parameter itself, then the type isn't the same.
                        //
                        // An example pathological case where this isn't the case is:
                        // `pub type Foo<const N: usize> = Underlying<N + 1 - 1>;`
                        // Detecting that this is the same expression requires that one of
                        // rustdoc or our code do const-evaluation here.
                        //
                        // Const expressions like this are currently only on nightly,
                        // so we can't test them on stable Rust at the moment.
                        //
                        // TODO: revisit this decision when const expressions in types are stable
                        c.expr.as_str()
                    }
                    rustdoc_types::GenericArg::Infer => return None,
                };
                if ty_generic.name.as_str() != arg_generic_name {
                    // The typedef params are not in the same order as the underlying type's.
                    return None;
                }

                match (&ty_generic.kind, &underlying_param.kind) {
                    (
                        rustdoc_types::GenericParamDefKind::Lifetime { .. },
                        rustdoc_types::GenericParamDefKind::Lifetime { .. },
                    ) => {
                        // Typedefs cannot have "outlives" relationships on their lifetimes,
                        // so there's nothing further to compare here. So far, it's a match.
                    }
                    (
                        rustdoc_types::GenericParamDefKind::Type {
                            default: ty_default,
                            ..
                        },
                        rustdoc_types::GenericParamDefKind::Type {
                            default: underlying_default,
                            ..
                        },
                    ) => {
                        // If the typedef doesn't have the same default values for its generics,
                        // then it isn't equivalent to the underlying and so isn't a re-export.
                        if ty_default != underlying_default {
                            // The defaults have changed.
                            return None;
                        }
                        // We don't care about the other fields.
                        // Generic bounds on typedefs are ignored by rustc and generate a lint.
                    }
                    (
                        rustdoc_types::GenericParamDefKind::Const {
                            type_: ty_type,
                            default: ty_default,
                        },
                        rustdoc_types::GenericParamDefKind::Const {
                            type_: underlying_type,
                            default: underlying_default,
                        },
                    ) => {
                        // If the typedef doesn't have the same default values for its generics,
                        // then it isn't equivalent to the underlying and so isn't a re-export.
                        //
                        // Similarly, if it is in any way possible to change the const generic type,
                        // that makes the typedef not a re-export anymore.
                        if ty_default != underlying_default || ty_type != underlying_type {
                            // The generic type or its default has changed.
                            return None;
                        }
                    }
                    _ => {
                        // Not the same kind of generic parameter.
                        return None;
                    }
                }
            }
        }

        Some(underlying)
    } else {
        None
    }
}
