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
        item: &'a Item,
        already_visited_ids: &mut HashSet<&'a Id>,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        let next_id = &item.id;
        if !already_visited_ids.insert(next_id) {
            // We found a cycle, and we've already processed this item.
            // Nothing more to do here.
            return;
        }

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

        self.collect_publicly_importable_names_inner(item, already_visited_ids, stack, output);

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
        next_item: &'a Item,
        already_visited_ids: &mut HashSet<&'a Id>,
        stack: &mut Vec<&'a str>,
        output: &mut Vec<Vec<&'a str>>,
    ) {
        if next_item.id == self.inner.root {
            let final_name = stack.iter().rev().copied().collect();
            output.push(final_name);
        } else if let Some(visible_parents) = self.visible_parent_ids.get(&next_item.id) {
            println!("{:?} {:?}", next_item.name, next_item.inner);
            println!("{:?} => {visible_parents:#?}", next_item.id);
            let is_glob_import = matches!(&next_item.inner, ItemEnum::Import(imp) if imp.glob);
            if is_glob_import {
                println!("found glob: {:#?}", next_item.inner);
            }
            for parent_id in visible_parents.iter().copied() {
                let parent_item = &self.inner.index[parent_id];

                let recurse_into_item = !is_glob_import
                    || 'recurse_into: {
                        // Check if the current leaf name conflicts with any explicitly-defined
                        // items in the parent scope.
                        let current_leaf_name = *stack.last().expect("found an empty stack");

                        dbg!((&next_item.name, &parent_item.name));
                        dbg!(current_leaf_name);
                        match &parent_item.inner {
                            ItemEnum::Module(m) => {
                                for contained_id in &m.items {
                                    if contained_id != &next_item.id {
                                        let contained_item = &self.inner.index[contained_id];
                                        if contained_item.name.as_deref() == Some(current_leaf_name)
                                        {
                                            break 'recurse_into false;
                                        }
                                    }
                                }
                            }
                            ItemEnum::Enum(e) => {
                                // TODO: test for enum variants glob imports not importing a variant
                                //       and test for it importing a variant in the presence of the same name in another namespace
                            }
                            _ => {}
                        }

                        true
                    };

                if recurse_into_item {
                    self.collect_publicly_importable_names(
                        parent_item,
                        already_visited_ids,
                        stack,
                        output,
                    );
                }
            }
        }
    }

    #[cfg(test)]
    pub(super) fn visible_parent_ids(&self) -> &HashMap<&'a Id, Vec<&'a Id>> {
        &self.visible_parent_ids
    }
}

fn compute_parent_ids_for_public_items(crate_: &Crate) -> HashMap<&Id, HashSet<&Id>> {
    let mut result = Default::default();
    let root_id = &crate_.root;
    if let Some(root_module) = crate_.index.get(root_id) {
        if root_module.visibility == Visibility::Public {
            let mut currently_visited_items = Default::default();
            visit_root_reachable_public_items(
                crate_,
                &mut result,
                &mut currently_visited_items,
                root_module,
                None,
            );
        }
    }

    result
}

/// Collect all public items that are reachable from the crate root and record their parent Ids.
fn visit_root_reachable_public_items<'a>(
    crate_: &'a Crate,
    parents: &mut HashMap<&'a Id, HashSet<&'a Id>>,
    currently_visited_items: &mut HashSet<&'a Id>,
    item: &'a Item,
    parent_id: Option<&'a Id>,
) {
    match item.visibility {
        Visibility::Crate => {
            if matches!(item.inner, ItemEnum::Impl(_)) {
                // A bug in rustdoc of Rust 1.69 and older causes `impl` items
                // to be given `crate` visibility instead of the correct `default` visibility.
                // Rust does not support `pub(crate) impl` or other visibility modifiers,
                // so if we're in this block, we're affected by the bug.
                //
                // The fix has shipped in 1.70 beta, but that still uses rustdoc v24.
                // TODO: Remove this in rustdoc v25+ since the fix should be present there.
            } else {
                // This item is not public, so we don't need to process it.
                return;
            }
        }
        Visibility::Restricted { .. } => {
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
                    currently_visited_items,
                    inner,
                    next_parent_id,
                );
            }
        }
        rustdoc_types::ItemEnum::Import(imp) => {
            // Imports of modules, and glob imports of enums,
            // import the *contents* of the pointed-to item rather than the item itself.
            if let Some(imported_item) = imp.id.as_ref().and_then(|id| crate_.index.get(id)) {
                if imp.glob {
                    // Glob imports point directly to the contents of the pointed-to module.
                    // For each item in that module, the import's parent becomes its parent as well.
                    let next_parent_id = parent_id;

                    let inner_ids = match &imported_item.inner {
                        rustdoc_types::ItemEnum::Module(mod_item) => &mod_item.items,
                        rustdoc_types::ItemEnum::Enum(enum_item) => &enum_item.variants,
                        _ => unreachable!(
                            "found a glob import of an unexpected kind of item: \
                            {imp:?} {imported_item:?}"
                        ),
                    };
                    for inner_id in inner_ids {
                        if let Some(inner_item) = crate_.index.get(inner_id) {
                            visit_root_reachable_public_items(
                                crate_,
                                parents,
                                currently_visited_items,
                                inner_item,
                                next_parent_id,
                            );
                        }
                    }
                } else {
                    visit_root_reachable_public_items(
                        crate_,
                        parents,
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
