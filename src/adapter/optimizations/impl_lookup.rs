use std::collections::HashMap;

use rustdoc_types::{Id, Item};
use trustfall::{
    provider::{
        resolve_neighbors_with, CandidateValue, ContextIterator, ContextOutcomeIterator,
        ResolveEdgeInfo, VertexInfo, VertexIterator,
    },
    FieldValue,
};

use crate::{indexed_crate::ImplEntry, IndexedCrate};

use super::super::{origin::Origin, vertex::Vertex, RustdocAdapter};

/// Resolve the `ImplOwner.impl` and `ImplOwner.inherent_impl` edges.
pub(crate) fn resolve_owner_impl<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    let current_crate = adapter.current_crate;
    let previous_crate = adapter.previous_crate;
    let inherent_impls_only = match edge_name {
        "inherent_impl" => true,
        "impl" => false,
        _ => unreachable!("unexpected edge name: {edge_name}"),
    };

    // Check if the `method` edge is used next at the destination.
    if let Some(method_vertex_info) = resolve_info
        .destination()
        .first_edge("method")
        .as_ref()
        .map(|e| e.destination())
    {
        // Try to use information about the `method` vertex to speed up the query.
        resolve_owner_impl_based_on_method_info(
            adapter,
            contexts,
            current_crate,
            previous_crate,
            inherent_impls_only,
            method_vertex_info,
        )
    } else {
        // We don't seem to be looking up methods. No fast path available.
        resolve_neighbors_with(contexts, move |vertex| {
            resolve_owner_impl_slow_path(vertex, current_crate, previous_crate, inherent_impls_only)
        })
    }
}

fn resolve_owner_impl_based_on_method_info<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
    inherent_impls_only: bool,
    method_vertex_info: &impl VertexInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    // Is the method's `name` property required to be some value, either statically or dynamically?
    // If so, we can use an index to look up a specific item directly.
    //
    // There's no advantage in our implementation between knowing values
    // statically vs dynamically, so we check the dynamic case first since
    // it might be more specific.
    if let Some(resolver) = method_vertex_info.dynamically_required_property("name") {
        resolver.resolve_with(adapter, contexts, move |vertex, candidate| {
            resolve_impl_based_on_method_name_candidate(
                vertex,
                current_crate,
                previous_crate,
                inherent_impls_only,
                candidate,
            )
        })
    } else if let Some(candidate) = method_vertex_info.statically_required_property("name") {
        let candidate = candidate.cloned();
        resolve_neighbors_with(contexts, move |vertex| {
            resolve_impl_based_on_method_name_candidate(
                vertex,
                current_crate,
                previous_crate,
                inherent_impls_only,
                candidate.clone(),
            )
        })
    } else {
        // The methods are not looked up by name. None of the fast paths are available.
        resolve_neighbors_with(contexts, move |vertex| {
            resolve_owner_impl_slow_path(vertex, current_crate, previous_crate, inherent_impls_only)
        })
    }
}

fn resolve_impl_based_on_method_name_candidate<'a>(
    vertex: &Vertex<'a>,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
    inherent_impls_only: bool,
    method_name: CandidateValue<FieldValue>,
) -> VertexIterator<'a, Vertex<'a>> {
    let origin = vertex.origin;
    let impl_index = match origin {
        Origin::CurrentCrate => current_crate
            .impl_index
            .as_ref()
            .expect("no impl index present"),
        Origin::PreviousCrate => previous_crate
            .expect("no previous crate provided")
            .impl_index
            .as_ref()
            .expect("no impl index provided"),
    };

    let item_id = &vertex.as_item().expect("not an item").id;
    match method_name {
        CandidateValue::Impossible => Box::new(std::iter::empty()),
        CandidateValue::Single(value) => {
            let method_name = value.as_str().expect("method name was not a string");
            resolve_impl_based_on_method_name(
                origin,
                impl_index,
                inherent_impls_only,
                item_id,
                method_name,
            )
        }
        CandidateValue::Multiple(values) => Box::new(values.into_iter().flat_map(move |value| {
            let method_name = value.as_str().expect("method name was not a string");
            resolve_impl_based_on_method_name(
                origin,
                impl_index,
                inherent_impls_only,
                item_id,
                method_name,
            )
        })),
        _ => {
            // fall through to slow path
            resolve_owner_impl_slow_path(vertex, current_crate, previous_crate, inherent_impls_only)
        }
    }
}

fn resolve_impl_based_on_method_name<'a>(
    origin: Origin,
    impl_index: &'a HashMap<ImplEntry<'a>, Vec<(&'a Item, &'a Item)>>,
    inherent_impls_only: bool,
    item_id: &Id,
    method_name: &str,
) -> VertexIterator<'a, Vertex<'a>> {
    if let Some(method_ids) = impl_index.get(&(item_id, method_name)) {
        Box::new(method_ids.iter().filter_map(move |(impl_item, _)| {
            let impl_content = match &impl_item.inner {
                rustdoc_types::ItemEnum::Impl(imp) => imp,
                _ => unreachable!(
                    "\
the `impl_index` returned a value where the `impl_item` was not an impl: {impl_item:?}"
                ),
            };
            if !inherent_impls_only || impl_content.trait_.is_none() {
                Some(origin.make_item_vertex(impl_item))
            } else {
                None
            }
        }))
    } else {
        Box::new(std::iter::empty())
    }
}

fn resolve_owner_impl_slow_path<'a>(
    vertex: &Vertex<'a>,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
    inherent_impls_only: bool,
) -> VertexIterator<'a, Vertex<'a>> {
    let origin = vertex.origin;
    let item_index = match origin {
        Origin::CurrentCrate => &current_crate.inner.index,
        Origin::PreviousCrate => {
            &previous_crate
                .expect("no previous crate provided")
                .inner
                .index
        }
    };

    // Get the IDs of all the impl blocks.
    // Relies on the fact that only structs and enums can have impls,
    // so we know that the vertex must represent either a struct or an enum.
    let impl_ids = vertex
        .as_struct()
        .map(|s| &s.impls)
        .or_else(|| vertex.as_enum().map(|e| &e.impls))
        .expect("vertex was neither a struct nor an enum");

    Box::new(impl_ids.iter().filter_map(move |item_id| {
        let next_item = item_index.get(item_id);
        next_item.and_then(|next_item| match &next_item.inner {
            rustdoc_types::ItemEnum::Impl(imp) => {
                if !inherent_impls_only || imp.trait_.is_none() {
                    Some(origin.make_item_vertex(next_item))
                } else {
                    None
                }
            }
            _ => None,
        })
    }))
}
