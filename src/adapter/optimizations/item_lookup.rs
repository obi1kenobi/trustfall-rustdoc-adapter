use std::sync::Arc;

use rustdoc_types::Item;
use trustfall::{
    FieldValue,
    provider::{
        AsVertex, CandidateValue, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo,
        VertexInfo, VertexIterator, resolve_neighbors_with,
    },
};

use super::super::{RustdocAdapter, origin::Origin, vertex::Vertex};

use crate::IndexedCrate;

pub(crate) fn resolve_crate_items<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    let destination = resolve_info.destination();

    // Is the `importable_path` edge being resolved in a mandatory fashion in a subsequent step?
    if let Some(neighbor_info) = destination
        .first_mandatory_edge("importable_path")
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
            return dynamic_value.resolve_with(&adapter, contexts, move |vertex, candidate| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(
                    crate_vertex,
                    origin,
                    destination.coerced_to_type().cloned(),
                    candidate,
                )
            });
        } else if let Some(path_value) = neighbor_info.statically_required_property("path") {
            return resolve_neighbors_with(contexts, move |vertex| {
                let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
                let origin = vertex.origin;
                resolve_items_by_importable_path(
                    crate_vertex,
                    origin,
                    destination.coerced_to_type().cloned(),
                    path_value.clone(),
                )
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

    // Is the `importable_path` edge being resolved in a subsequent step in a *mandatory* fashion?
    // If so, we could only match on public items, so check which kinds of public items
    // we're looking for and only return those from the index.
    if destination
        .first_mandatory_edge("importable_path")
        .is_some()
    {
        if let Some(item_type) = destination.coerced_to_type().map(|x| x.as_ref()) {
            match item_type {
                "Function" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .free_functions
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Struct" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .structs
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Enum" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .enums
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Union" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .unions
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Trait" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .traits
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "ImplOwner" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .structs
                                .values()
                                .chain(crate_vertex.pub_item_kind_index.enums.values())
                                .chain(crate_vertex.pub_item_kind_index.unions.values())
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Constant" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .free_consts
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Static" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .statics
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "GlobalValue" => {
                    // const or static
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .free_consts
                                .values()
                                .chain(crate_vertex.pub_item_kind_index.statics.values())
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Macro" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .decl_macros
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "ProcMacro"
                | "FunctionLikeProcMacro"
                | "AttributeProcMacro"
                | "DeriveProcMacro" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .proc_macros
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                "Module" => {
                    return resolve_neighbors_with(contexts, move |vertex| {
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");
                        let origin = vertex.origin;
                        Box::new(
                            crate_vertex
                                .pub_item_kind_index
                                .modules
                                .values()
                                .map(move |item| origin.make_item_vertex(item)),
                        )
                    });
                }
                _ => {}
            }
        }
    }

    resolve_neighbors_with(contexts, |vertex| {
        let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");
        let origin = vertex.origin;
        resolve_items_slow_path(crate_vertex, origin)
    })
}

/// Resolve public items with candidate value `importable_path` and type `destination_type`.
///
/// If the destination is None or an unrecognised string, we conservatively return all
/// paths that match the candidate value.
fn resolve_items_by_importable_path<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
    destination_type: Option<Arc<str>>,
    importable_path: CandidateValue<FieldValue>,
) -> VertexIterator<'a, Vertex<'a>> {
    match importable_path {
        CandidateValue::Impossible => Box::new(std::iter::empty()),
        CandidateValue::Single(value) => resolve_items_by_importable_path_field_value(
            crate_vertex,
            origin,
            destination_type.as_deref(),
            &value,
        ),
        CandidateValue::Multiple(values) => Box::new(values.into_iter().flat_map(move |value| {
            resolve_items_by_importable_path_field_value(
                crate_vertex,
                origin,
                destination_type.as_deref(),
                &value,
            )
        })),
        _ => {
            // fall through to slow path
            resolve_items_slow_path(crate_vertex, origin)
        }
    }
}

/// Resolve public items with importable path `path`, optionally of vertex type `destination_type`.
///
/// For example, "structs at path `foo::bar`" or "anything at `foo::bar`".
/// The former has destination type `Some("Struct")`, while the latter has `None`.
///
/// When the destination is `None` or the name of a type that we don't have an index for,
/// we conservatively return all paths that match the `value`.
fn resolve_items_by_importable_path_field_value<'a>(
    crate_vertex: &'a IndexedCrate,
    origin: Origin,
    destination_type: Option<&str>,
    path: &FieldValue,
) -> VertexIterator<'a, Vertex<'a>> {
    let path_components: Vec<&str> = path
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
        let base_iter = items.iter().map(|(item, _)| item).copied();
        if let Some(destination_type) = destination_type {
            match destination_type {
                "Function" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .free_functions
                            .contains_key(&item.id)
                    }),
                ),
                "Struct" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .structs
                            .contains_key(&item.id)
                    }),
                ),
                "Enum" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .enums
                            .contains_key(&item.id)
                    }),
                ),
                "Union" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .unions
                            .contains_key(&item.id)
                    }),
                ),
                "Trait" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .traits
                            .contains_key(&item.id)
                    }),
                ),
                "ImplOwner" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .structs
                            .contains_key(&item.id)
                            || crate_vertex
                                .pub_item_kind_index
                                .enums
                                .contains_key(&item.id)
                            || crate_vertex
                                .pub_item_kind_index
                                .unions
                                .contains_key(&item.id)
                    }),
                ),
                "Constant" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .free_consts
                            .contains_key(&item.id)
                    }),
                ),
                "Static" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .statics
                            .contains_key(&item.id)
                    }),
                ),
                "GlobalValue" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        // const or static
                        crate_vertex
                            .pub_item_kind_index
                            .free_consts
                            .contains_key(&item.id)
                            || crate_vertex
                                .pub_item_kind_index
                                .statics
                                .contains_key(&item.id)
                    }),
                ),
                "Macro" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .decl_macros
                            .contains_key(&item.id)
                    }),
                ),
                "ProcMacro"
                | "FunctionLikeProcMacro"
                | "AttributeProcMacro"
                | "DeriveProcMacro" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .proc_macros
                            .contains_key(&item.id)
                    }),
                ),
                "Module" => resolve_item_vertices(
                    origin,
                    base_iter.filter(move |item| {
                        crate_vertex
                            .pub_item_kind_index
                            .modules
                            .contains_key(&item.id)
                    }),
                ),
                _ => {
                    // No index is available for this type.
                    //
                    // If this branch is reached inside time sensitive code, consider
                    // adding the destination type to an index.
                    resolve_item_vertices(origin, base_iter)
                }
            }
        } else {
            // This query doesn't apply a coercion on the resulting vertex,
            // so we produce all vertices that matched the path lookup.
            resolve_item_vertices(origin, base_iter)
        }
    } else {
        // No items at found at the given path.
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
