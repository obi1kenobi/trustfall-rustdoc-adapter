use rustdoc_types::Item;
use trustfall::{
    provider::{
        resolve_neighbors_with, AsVertex, CandidateValue, ContextIterator, ContextOutcomeIterator,
        ResolveEdgeInfo, VertexInfo, VertexIterator,
    },
    FieldValue,
};

use super::super::{origin::Origin, vertex::Vertex, RustdocAdapter};

use crate::IndexedCrate;

pub(crate) fn resolve_crate_items<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    let destination = resolve_info.destination();

    // Is the `importable_path` edge being resolved in a subsequent step?
    if let Some(neighbor_info) = destination
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
            return dynamic_value.resolve_with(&adapter, contexts, |vertex, candidate| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(crate_vertex, origin, candidate)
            });
        } else if let Some(path_value) = neighbor_info.statically_required_property("path") {
            return resolve_neighbors_with(contexts, move |vertex| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(crate_vertex, origin, path_value.clone())
            });
        }
    }

    // For this edge, normally the destination is of type `Item`.
    // But if the destination is coerced to type `Function`, and is then filtered on `export_name`,
    // we have another index we can apply to look up the relevant functions directly.
    if destination.coerced_to_type().map(|x| x.as_ref()) == Some("Function") {
        if let Some(candidate) = destination.statically_required_property("export_name") {
            return resolve_neighbors_with(contexts, move |vertex| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_function_by_export_name(crate_vertex, origin, candidate.clone())
            });
        } else if let Some(dynamic_value) = destination.dynamically_required_property("export_name")
        {
            return dynamic_value.resolve_with(&adapter, contexts, |vertex, candidate| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_function_by_export_name(crate_vertex, origin, candidate)
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
        resolve_item_vertices(origin, items.iter().map(|(item, _)| item).copied())
    } else {
        // No such items found.
        Box::new(std::iter::empty())
    }
}

fn resolve_function_by_export_name<'a>(
    crate_vertex: &'a IndexedCrate<'a>,
    origin: Origin,
    export_name: CandidateValue<FieldValue>,
) -> VertexIterator<'a, Vertex<'a>> {
    match export_name {
        CandidateValue::Impossible => Box::new(std::iter::empty()),
        CandidateValue::Single(value) => Box::new(
            resolve_function_by_export_name_field_value(crate_vertex, origin, &value).into_iter(),
        ),
        CandidateValue::Multiple(values) => Box::new(values.into_iter().filter_map(move |value| {
            resolve_function_by_export_name_field_value(crate_vertex, origin, &value)
        })),
        _ => {
            // fall through to slow path
            resolve_items_slow_path(crate_vertex, origin)
        }
    }
}

fn resolve_function_by_export_name_field_value<'a>(
    crate_vertex: &'a IndexedCrate<'a>,
    origin: Origin,
    export_name: &FieldValue,
) -> Option<Vertex<'a>> {
    match export_name {
        FieldValue::String(export_name) => crate_vertex
            .export_name_index
            .as_ref()
            .expect("export name index was never built")
            .get(export_name.as_ref())
            .map(|item| origin.make_item_vertex(item)),
        _ => None,
    }
}

fn resolve_items_slow_path<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
) -> VertexIterator<'a, Vertex<'a>> {
    // When listing the items in the crate index, ensure we return
    // only the items that belong to the crate itself.
    // This is a concern since the crate index in rustdoc JSON can sometimes contain
    // inlined items from language builtins like `str`:
    // https://rust-lang.zulipchat.com/#narrow/stream/266220-rustdoc/topic/Rustdoc.20JSON.3A.20Unexpected.20.60core.60.20items.20included.20in.20output/near/377420065
    //
    // We look up the `crate_id` of the root module of the crate, and then discard any items
    // that don't belong to that same `crate_id`. This matches the fast-path behavior.
    let own_crate_id = crate_vertex.inner.index[&crate_vertex.inner.root].crate_id;
    let items = crate_vertex
        .inner
        .index
        .values()
        .filter(move |item| item.crate_id == own_crate_id)
        .filter(move |item| {
            // We don't consider methods as top-level items in a crate.
            // We should only return methods as owned items within a trait or impl.
            // If we are to return this item, then either the item isn't a function at all,
            // or it's a top-level function (i.e. has no owner listed in the index).
            !matches!(item.inner, rustdoc_types::ItemEnum::Function(..))
                || crate_vertex
                    .fn_owner_index
                    .as_ref()
                    .expect("no fn_owner_index defined")
                    .get(&item.id)
                    .is_none()
        });

    resolve_item_vertices(origin, items)
}

fn resolve_item_vertices<'a>(
    origin: Origin,
    items: impl Iterator<Item = &'a Item> + 'a,
) -> VertexIterator<'a, Vertex<'a>> {
    Box::new(
        items
            .filter(|value| crate::adapter::supported_item_kind(value))
            .map(move |value| origin.make_item_vertex(value)),
    )
}
