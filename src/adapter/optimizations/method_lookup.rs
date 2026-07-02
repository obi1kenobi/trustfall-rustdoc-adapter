use rustdoc_types::{Id, Impl, Item, ItemEnum, Type};
use trustfall::{
    FieldValue,
    provider::{
        AsVertex, CandidateValue, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo,
        VertexInfo, VertexIterator, resolve_neighbors_with,
    },
};

use crate::{
    RustdocAdapter,
    adapter::{Origin, Vertex},
    hashtables::{HashMap, HashSet},
    indexed_crate::ImplEntry,
    stability::PublicApiStabilityPolicy,
};

pub(crate) fn resolve_impl_methods<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    let neighbor_info = resolve_info.destination();

    // Is the `name` value within that edge known, either statically or dynamically?
    // If so, we can use an index to look up a specific method directly.
    //
    // There's no advantage in our implementation between knowing values
    // statically vs dynamically, so we check the dynamic case first since
    // it might be more specific.
    if let Some(resolver) = neighbor_info.dynamically_required_property("name") {
        resolver.resolve_with(&adapter, contexts, move |vertex, candidate| {
            resolve_method_from_candidate_value(adapter, vertex, candidate)
        })
    } else if let Some(candidate) = neighbor_info.statically_required_property("name") {
        resolve_neighbors_with(contexts, move |vertex| {
            resolve_method_from_candidate_value(adapter, vertex, candidate.clone())
        })
    } else {
        resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let indexed_crate = &adapter.crate_at_origin(origin).own_crate;
            let item_index = &indexed_crate.inner.index;

            let impl_item = vertex.as_item().expect("not an Impl item");
            let impl_vertex = vertex.as_impl().expect("not an Impl vertex");
            resolve_methods_slow_path(
                impl_item,
                impl_vertex,
                origin,
                item_index,
                indexed_crate.stability_policy,
            )
        })
    }
}

fn find_impl_owner_id(impl_vertex: &Impl) -> Option<&Id> {
    let mut ty = &impl_vertex.for_;
    loop {
        match ty {
            Type::ResolvedPath(path) => break Some(&path.id),
            Type::BorrowedRef {
                lifetime: _,
                is_mutable: _,
                type_,
            } => {
                ty = type_;
            }
            Type::RawPointer {
                is_mutable: _,
                type_,
            } => {
                ty = type_;
            }
            _ => {
                // We encountered an `impl <X>` or `impl Trait for <X>` with an unexpected `<X>`.
                // Ideally, this case would never happen.
                // But since it did, let's fall back to the slow path.
                break None;
            }
        }
    }
}

fn resolve_method_from_candidate_value<'a>(
    adapter: &'a RustdocAdapter<'a>,
    vertex: &Vertex<'a>,
    method_name: CandidateValue<FieldValue>,
) -> VertexIterator<'a, Vertex<'a>> {
    let origin = vertex.origin;
    let indexed_crate = &adapter.crate_at_origin(origin).own_crate;
    let item_index = &indexed_crate.inner.index;
    let impl_index = indexed_crate
        .impl_method_index
        .as_ref()
        .expect("no impl index provided");

    let impl_id = &vertex.as_item().expect("not an Item vertex").id;
    let impl_vertex = vertex.as_impl().expect("not an Impl vertex");

    if let Some(impl_owner_id) = find_impl_owner_id(impl_vertex) {
        match method_name {
            CandidateValue::Impossible => Box::new(std::iter::empty()),
            CandidateValue::Single(name) => {
                let method_name = name.as_str().expect("method name was not a string");
                resolve_impl_method_by_name(
                    origin,
                    impl_index,
                    item_index,
                    impl_owner_id,
                    impl_id,
                    method_name,
                )
            }
            CandidateValue::Multiple(names) => Box::new(names.into_iter().flat_map(move |name| {
                let method_name = name.as_str().expect("method name was not a string");
                resolve_impl_method_by_name(
                    origin,
                    impl_index,
                    item_index,
                    impl_owner_id,
                    impl_id,
                    method_name,
                )
            })),
            _ => {
                // Fall back to the default slow path.
                let impl_item = vertex.as_item().expect("not an Impl item");
                resolve_methods_slow_path(
                    impl_item,
                    impl_vertex,
                    origin,
                    item_index,
                    indexed_crate.stability_policy,
                )
            }
        }
    } else {
        // We couldn't determine the Id of the item that owns this method.
        // Fall back to the default slow path.
        let impl_item = vertex.as_item().expect("not an Impl item");
        resolve_methods_slow_path(
            impl_item,
            impl_vertex,
            origin,
            item_index,
            indexed_crate.stability_policy,
        )
    }
}

fn resolve_impl_method_by_name<'a>(
    origin: Origin,
    impl_index: &'a HashMap<ImplEntry<'a>, Vec<(&'a Item, &'a Item)>>,
    item_index: &'a HashMap<Id, Item>,
    impl_owner_id: &'a Id,
    impl_id: &'a Id,
    method_name: &str,
) -> VertexIterator<'a, Vertex<'a>> {
    if let Some(method_ids) = impl_index.get(&(impl_owner_id, method_name)) {
        Box::new(
            method_ids
                .iter()
                .filter(move |(impl_item, _)| &impl_item.id == impl_id)
                .map(move |(impl_item, item)| {
                    let parent = method_parent_for_impl_method(impl_item, item, item_index);
                    origin.make_method_vertex(item, parent)
                }),
        )
    } else {
        Box::new(std::iter::empty())
    }
}

fn method_parent_for_impl_method<'a>(
    impl_item: &'a Item,
    method_item: &'a Item,
    item_index: &'a HashMap<Id, Item>,
) -> &'a Item {
    let ItemEnum::Impl(impl_) = &impl_item.inner else {
        unreachable!("method index returned a non-impl parent item: {impl_item:?}");
    };

    // Return the function declaration parent, not necessarily the `impl` we
    // traversed from. Explicit impl methods use the `impl` generic scope, while
    // trait-provided default methods use the trait generic scope because the
    // rustdoc function item is declared on the trait.
    if impl_.items.contains(&method_item.id) {
        return impl_item;
    }

    let method_name = method_item
        .name
        .as_deref()
        .expect("method item had no name");
    debug_assert!(
        impl_
            .provided_trait_methods
            .iter()
            .any(|name| name == method_name),
        "method item was neither explicitly implemented nor trait-provided: {method_item:?}",
    );

    let trait_path = impl_
        .trait_
        .as_ref()
        .expect("provided trait method appeared on an inherent impl");
    let trait_item = item_index
        .get(&trait_path.id)
        .expect("provided trait method parent trait was missing from the item index");
    let ItemEnum::Trait(trait_) = &trait_item.inner else {
        unreachable!("provided method parent was not a trait: {trait_item:?}");
    };
    debug_assert!(
        trait_.items.contains(&method_item.id),
        "provided method was not listed on its parent trait: {method_item:?}",
    );
    trait_item
}

fn resolve_methods_slow_path<'a>(
    impl_item: &'a Item,
    impl_vertex: &'a Impl,
    origin: Origin,
    item_index: &'a HashMap<Id, Item>,
    stability_policy: PublicApiStabilityPolicy,
) -> VertexIterator<'a, Vertex<'a>> {
    let provided_methods: Box<dyn Iterator<Item = &Id>> =
        if impl_vertex.provided_trait_methods.is_empty() {
            Box::new(std::iter::empty())
        } else {
            let method_names: HashSet<&str> = impl_vertex
                .provided_trait_methods
                .iter()
                .map(|x| x.as_str())
                .collect();

            let trait_path = impl_vertex
                .trait_
                .as_ref()
                .expect("no trait but provided_trait_methods was non-empty");
            let trait_item = item_index.get(&trait_path.id);

            if let Some(trait_item) = trait_item {
                if let ItemEnum::Trait(trait_item) = &trait_item.inner {
                    Box::new(trait_item.items.iter().filter(move |item_id| {
                        let Some(next_item) = item_index.get(item_id) else {
                            return false;
                        };
                        let rustdoc_types::ItemEnum::Function(function) = &next_item.inner else {
                            return false;
                        };
                        let Some(name) = next_item.name.as_deref() else {
                            return false;
                        };

                        method_names.contains(name)
                            && stability_policy.effective_function_has_body(function)
                    }))
                } else {
                    unreachable!("found a non-trait type {trait_item:?}");
                }
            } else {
                Box::new(std::iter::empty())
            }
        };

    let mut produced_methods: HashSet<&str> = Default::default();
    Box::new(
        // Iterate through explicitly-implemented items first, and trait-provided items next.
        // This ensures we prefer the explicitly-implemented method in cases where
        // the trait also provided a default impl (which is overridden and not used).
        impl_vertex
            .items
            .iter()
            .chain(provided_methods)
            .filter_map(move |item_id| {
                let next_item = &item_index.get(item_id);

                if let Some(next_item) = next_item {
                    let item_name = next_item.name.as_deref()?;
                    match &next_item.inner {
                        rustdoc_types::ItemEnum::Function(..) => {
                            // Ensure our iterator doesn't produce duplicate method names
                            // in the case where a trait provided a default
                            // but the impl had an override.
                            if produced_methods.insert(item_name) {
                                let parent =
                                    method_parent_for_impl_method(impl_item, next_item, item_index);
                                Some(origin.make_method_vertex(next_item, parent))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }),
    )
}
