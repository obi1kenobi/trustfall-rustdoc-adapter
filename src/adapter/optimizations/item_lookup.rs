use rustdoc_types::Item;
use trustfall::{
    provider::{
        resolve_neighbors_with, CandidateValue, ContextIterator, ContextOutcomeIterator,
        ResolveEdgeInfo, VertexInfo, VertexIterator,
    },
    FieldValue,
};

use super::super::{origin::Origin, vertex::Vertex, RustdocAdapter};

use crate::IndexedCrate;

pub(crate) fn resolve_crate_items<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    // Is the `importable_path` edge being resolved in a subsequent step?
    if let Some(neighbor_info) = resolve_info
        .destination()
        .first_edge("importable_path")
        .as_ref()
        .map(|x| x.destination())
    {
        // Is the `path` value within that edge known, either statically or dynamically?
        // If so, we can use an index to look up a specific item directly.
        //
        // There's no advantage in our implementation between knowing values
        // statically vs dynamically, so we check the dynamic case first since
        // it might be more specific.
        if let Some(dynamic_value) = neighbor_info.dynamically_required_property("path") {
            return dynamic_value.resolve_with(adapter, contexts, |vertex, candidate| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(crate_vertex, origin, candidate)
            });
        } else if let Some(path_value) = neighbor_info.statically_required_property("path") {
            let path_value = path_value.cloned();
            return resolve_neighbors_with(contexts, move |vertex| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(crate_vertex, origin, path_value.clone())
            });
        }
    }

    resolve_neighbors_with(contexts, |vertex| {
        let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
        let origin = vertex.origin;
        resolve_items_slow_path(crate_vertex, origin)
    })
}

fn resolve_items_by_importable_path<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
    importable_path: CandidateValue<FieldValue>,
) -> VertexIterator<'a, Vertex<'a>> {
    match importable_path {
        CandidateValue::Impossible => Box::new(std::iter::empty()),
        CandidateValue::Single(value) => {
            resolve_items_by_importable_path_field_value(crate_vertex, origin, &value)
        }
        CandidateValue::Multiple(values) => Box::new(values.into_iter().flat_map(move |value| {
            resolve_items_by_importable_path_field_value(crate_vertex, origin, &value)
        })),
        _ => {
            // fall through to slow path
            resolve_items_slow_path(crate_vertex, origin)
        }
    }
}

fn resolve_items_by_importable_path_field_value<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
    value: &FieldValue,
) -> VertexIterator<'a, Vertex<'a>> {
    let path_components: Vec<&str> = value
        .as_slice()
        .expect("ImportablePath.path was not a list")
        .iter()
        .map(|x| x.as_str().unwrap())
        .collect();
    if let Some(items) = crate_vertex
        .imports_index
        .as_ref()
        .expect("crate's imports_index was never constructed")
        .get(path_components.as_slice())
    {
        resolve_item_vertices(origin, items.iter().copied())
    } else {
        // No such items found.
        Box::new(std::iter::empty())
    }
}

fn resolve_items_slow_path<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
) -> VertexIterator<'a, Vertex<'a>> {
    resolve_item_vertices(origin, crate_vertex.inner.index.values())
}

fn resolve_item_vertices<'a>(
    origin: Origin,
    items: impl Iterator<Item = &'a Item> + 'a,
) -> VertexIterator<'a, Vertex<'a>> {
    Box::new(
        items
            .filter(|item| {
                // Filter out item types that are not currently supported.
                matches!(
                    item.inner,
                    rustdoc_types::ItemEnum::Struct(..)
                        | rustdoc_types::ItemEnum::StructField(..)
                        | rustdoc_types::ItemEnum::Enum(..)
                        | rustdoc_types::ItemEnum::Variant(..)
                        | rustdoc_types::ItemEnum::Function(..)
                        | rustdoc_types::ItemEnum::Method(..)
                        | rustdoc_types::ItemEnum::Impl(..)
                        | rustdoc_types::ItemEnum::Trait(..)
                )
            })
            .map(move |value| origin.make_item_vertex(value)),
    )
}
