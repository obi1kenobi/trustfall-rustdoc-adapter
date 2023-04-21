use trustfall::provider::{
    CandidateValue, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo, VertexInfo,
    VertexIterator,
};

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

    if let Some(resolver) = resolve_info
        .destination()
        .first_edge("method")
        .as_ref()
        .and_then(|y| y.destination().dynamically_required_property("name"))
    {
        Box::new(
            resolver
                .resolve(adapter, contexts)
                .map(move |(ctx, method_name)| {
                    let neighbors: Box<dyn Iterator<Item = _>> = match ctx.active_vertex() {
                        None => Box::new(std::iter::empty()),
                        Some(vertex) => {
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
                                    let method_name =
                                        value.as_str().expect("method name was not a string");
                                    if let Some(method_ids) =
                                        impl_index.get(&(item_id, method_name))
                                    {
                                        Box::new(method_ids.clone().into_iter().filter_map(
                                            move |(impl_item, _)| {
                                                let impl_content = match &impl_item.inner {
                                                    rustdoc_types::ItemEnum::Impl(imp) => imp,
                                                    _ => unreachable!(),
                                                };
                                                if !inherent_impls_only
                                                    || impl_content.trait_.is_none()
                                                {
                                                    Some(origin.make_item_vertex(impl_item))
                                                } else {
                                                    None
                                                }
                                            },
                                        ))
                                    } else {
                                        Box::new(std::iter::empty())
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                    };

                    (ctx, neighbors)
                }),
        )
    } else {
        Box::new(contexts.map(move |ctx| {
            let neighbors: Box<dyn Iterator<Item = Vertex<'a>> + 'a> = match ctx.active_vertex() {
                None => Box::new(std::iter::empty()),
                Some(vertex) => {
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
            };

            (ctx, neighbors)
        }))
    }
}
