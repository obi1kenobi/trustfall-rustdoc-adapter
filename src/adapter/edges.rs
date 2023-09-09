use rustdoc_types::{GenericBound::TraitBound, Id, ItemEnum, VariantKind};
use trustfall::provider::{
    resolve_neighbors_with, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo,
    VertexIterator,
};

use crate::{attributes::Attribute, IndexedCrate};

use super::{optimizations, origin::Origin, vertex::Vertex, RustdocAdapter};

pub(super) fn resolve_crate_diff_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "current" => resolve_neighbors_with(contexts, |vertex| {
            let crate_tuple = vertex.as_crate_diff().expect("vertex was not a CrateDiff");
            let neighbor = Vertex::new_crate(Origin::CurrentCrate, crate_tuple.0);
            Box::new(std::iter::once(neighbor))
        }),
        "baseline" => resolve_neighbors_with(contexts, |vertex| {
            let crate_tuple = vertex.as_crate_diff().expect("vertex was not a CrateDiff");
            let neighbor = Vertex::new_crate(Origin::PreviousCrate, crate_tuple.1);
            Box::new(std::iter::once(neighbor))
        }),
        _ => unreachable!("resolve_crate_diff_edge {edge_name}"),
    }
}

pub(super) fn resolve_crate_edge<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "item" => optimizations::item_lookup::resolve_crate_items(adapter, contexts, resolve_info),
        _ => unreachable!("resolve_crate_edge {edge_name}"),
    }
}

pub(super) fn resolve_importable_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "canonical_path" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            let item_id = &item.id;

            if let Some(path) = match origin {
                Origin::CurrentCrate => current_crate.inner.paths.get(item_id).map(|x| &x.path),
                Origin::PreviousCrate => previous_crate
                    .expect("no baseline provided")
                    .inner
                    .paths
                    .get(item_id)
                    .map(|x| &x.path),
            } {
                Box::new(std::iter::once(origin.make_path_vertex(path)))
            } else {
                Box::new(std::iter::empty())
            }
        }),
        "importable_path" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            let item_id = &item.id;

            let parent_crate = match origin {
                Origin::CurrentCrate => current_crate,
                Origin::PreviousCrate => previous_crate.expect("no baseline provided"),
            };

            Box::new(
                parent_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(move |x| origin.make_importable_path_vertex(x)),
            )
        }),
        _ => unreachable!("resolve_importable_edge {edge_name}"),
    }
}

pub(super) fn resolve_item_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "span" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            if let Some(span) = &item.span {
                Box::new(std::iter::once(origin.make_span_vertex(span)))
            } else {
                Box::new(std::iter::empty())
            }
        }),
        "attribute" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            Box::new(
                item.attrs
                    .iter()
                    .map(move |attr| origin.make_attribute_vertex(Attribute::new(attr.as_str()))),
            )
        }),
        _ => unreachable!("resolve_item_edge {edge_name}"),
    }
}

pub(super) fn resolve_impl_owner_edge<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "impl" | "inherent_impl" => optimizations::impl_lookup::resolve_owner_impl(
            adapter,
            contexts,
            edge_name,
            resolve_info,
        ),
        _ => unreachable!("resolve_impl_owner_edge {edge_name}"),
    }
}

pub(super) fn resolve_function_like_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "parameter" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            Box::new(
                vertex
                    .as_function()
                    .expect("vertex was not a Function")
                    .decl
                    .inputs
                    .iter()
                    .map(move |(name, _type_)| origin.make_function_parameter_vertex(name)),
            )
        }),
        "abi" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let abi = &vertex
                .as_function()
                .expect("vertex was not a Function")
                .header
                .abi;

            Box::new(std::iter::once(origin.make_function_abi_vertex(abi)))
        }),
        _ => unreachable!("resolve_function_like_edge {edge_name}"),
    }
}

pub(super) fn resolve_module_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "item" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let module_item = vertex.as_module().expect("vertex was not a Module");

            let item_index = match origin {
                Origin::CurrentCrate => &current_crate.inner.index,
                Origin::PreviousCrate => {
                    &previous_crate
                        .expect("no previous crate provided")
                        .inner
                        .index
                }
            };

            Box::new(module_item.items.iter().filter_map(move |item_id| {
                item_index
                    .get(item_id)
                    .map(|item| origin.make_item_vertex(item))
            }))
        }),
        _ => unreachable!("resolve_module_edge {edge_name}"),
    }
}

pub(super) fn resolve_struct_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "field" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let struct_item = vertex.as_struct().expect("vertex was not a Struct");

            let item_index = match origin {
                Origin::CurrentCrate => &current_crate.inner.index,
                Origin::PreviousCrate => {
                    &previous_crate
                        .expect("no previous crate provided")
                        .inner
                        .index
                }
            };

            let field_ids_iter: Box<dyn Iterator<Item = &Id>> = match &struct_item.kind {
                rustdoc_types::StructKind::Unit => Box::new(std::iter::empty()),
                rustdoc_types::StructKind::Tuple(field_ids) => {
                    Box::new(field_ids.iter().filter_map(|x| x.as_ref()))
                }
                rustdoc_types::StructKind::Plain { fields, .. } => Box::new(fields.iter()),
            };

            Box::new(field_ids_iter.map(move |field_id| {
                origin.make_item_vertex(item_index.get(field_id).expect("missing item"))
            }))
        }),
        _ => unreachable!("resolve_struct_edge {edge_name}"),
    }
}

pub(super) fn resolve_variant_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "field" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_variant().expect("vertex was not a Variant");
            let item_index = match origin {
                Origin::CurrentCrate => &current_crate.inner.index,
                Origin::PreviousCrate => {
                    &previous_crate
                        .expect("no previous crate provided")
                        .inner
                        .index
                }
            };

            match &item.kind {
                VariantKind::Plain => Box::new(std::iter::empty()),
                VariantKind::Tuple(fields) => {
                    Box::new(fields.iter().filter(|x| x.is_some()).map(move |field_id| {
                        origin.make_item_vertex(
                            item_index
                                .get(field_id.as_ref().unwrap())
                                .expect("missing item"),
                        )
                    }))
                }
                VariantKind::Struct {
                    fields,
                    fields_stripped: _,
                } => Box::new(fields.iter().map(move |field_id| {
                    origin.make_item_vertex(item_index.get(field_id).expect("missing item"))
                })),
            }
        }),
        _ => unreachable!("resolve_variant_edge {edge_name}"),
    }
}

pub(super) fn resolve_enum_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "variant" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let enum_item = vertex.as_enum().expect("vertex was not an Enum");

            let item_index = match origin {
                Origin::CurrentCrate => &current_crate.inner.index,
                Origin::PreviousCrate => {
                    &previous_crate
                        .expect("no previous crate provided")
                        .inner
                        .index
                }
            };
            Box::new(enum_item.variants.iter().map(move |field_id| {
                origin.make_item_vertex(item_index.get(field_id).expect("missing item"))
            }))
        }),
        _ => unreachable!("resolve_enum_edge {edge_name}"),
    }
}

pub(super) fn resolve_struct_field_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "raw_type" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let field_type = vertex.as_struct_field().expect("not a StructField vertex");
            Box::new(std::iter::once(origin.make_raw_type_vertex(field_type)))
        }),
        _ => unreachable!("resolve_struct_field_edge {edge_name}"),
    }
}

pub(super) fn resolve_impl_edge<'a>(
    adapter: &RustdocAdapter<'a>,
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    let current_crate = adapter.current_crate;
    let previous_crate = adapter.previous_crate;
    match edge_name {
        "method" => {
            optimizations::method_lookup::resolve_impl_methods(adapter, contexts, resolve_info)
        }
        "implemented_trait" => resolve_neighbors_with(contexts, move |vertex| {
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

            let impl_vertex = vertex.as_impl().expect("not an Impl vertex");

            if let Some(path) = &impl_vertex.trait_ {
                // When the implemented trait is from the same crate
                // as its definition, the trait is expected to be present
                // in `item_index`. Otherwise, the
                // `rustdoc_types::Trait` is not in this rustdoc,
                // even if the trait is part of Rust `core` or `std`.
                // As a temporary workaround, some common
                // Rust built-in traits are manually "inlined"
                // with items stored in `manually_inlined_builtin_traits`.
                let found_item = item_index.get(&path.id).or_else(|| {
                    let manually_inlined_builtin_traits = match origin {
                        Origin::CurrentCrate => &current_crate.manually_inlined_builtin_traits,
                        Origin::PreviousCrate => {
                            &previous_crate
                                .expect("no previous crate provided")
                                .manually_inlined_builtin_traits
                        }
                    };
                    manually_inlined_builtin_traits.get(&path.id)
                });
                if let Some(item) = found_item {
                    Box::new(std::iter::once(
                        origin.make_implemented_trait_vertex(path, item),
                    ))
                } else {
                    Box::new(std::iter::empty())
                }
            } else {
                Box::new(std::iter::empty())
            }
        }),
        "associated_constant" => resolve_neighbors_with(contexts, move |vertex| {
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

            let impl_vertex = vertex.as_impl().expect("not an Impl vertex");
            Box::new(impl_vertex.items.iter().filter_map(move |item_id| {
                if let Some(item) = item_index.get(item_id) {
                    matches!(item.inner, ItemEnum::AssocConst { .. })
                        .then(|| origin.make_item_vertex(item))
                } else {
                    None
                }
            }))
        }),
        _ => unreachable!("resolve_impl_edge {edge_name}"),
    }
}

pub(super) fn resolve_trait_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "supertrait" => resolve_neighbors_with(contexts, move |vertex| {
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

            let trait_vertex = vertex.as_trait().expect("not a Trait vertex");
            Box::new(trait_vertex.bounds.iter().filter_map(move |bound| {
                if let TraitBound { trait_, .. } = bound {
                    item_index
                        .get(&trait_.id)
                        .as_ref()
                        .map(|next_item| origin.make_implemented_trait_vertex(trait_, next_item))
                } else {
                    None
                }
            }))
        }),
        "method" => resolve_neighbors_with(contexts, move |vertex| {
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

            let trait_vertex = vertex.as_trait().expect("not a Trait vertex");
            Box::new(trait_vertex.items.iter().filter_map(move |item_id| {
                let next_item = &item_index.get(item_id);
                if let Some(next_item) = next_item {
                    match &next_item.inner {
                        rustdoc_types::ItemEnum::Function(..) => {
                            Some(origin.make_item_vertex(next_item))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }))
        }),
        "associated_type" => resolve_neighbors_with(contexts, move |vertex| {
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

            let trait_vertex = vertex.as_trait().expect("not a Trait vertex");
            Box::new(trait_vertex.items.iter().filter_map(move |item_id| {
                let next_item = &item_index.get(item_id);
                if let Some(next_item) = next_item {
                    match &next_item.inner {
                        rustdoc_types::ItemEnum::AssocType { .. } => {
                            Some(origin.make_item_vertex(next_item))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }))
        }),
        "associated_constant" => resolve_neighbors_with(contexts, move |vertex| {
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

            let trait_vertex = vertex.as_trait().expect("not a Trait vertex");
            Box::new(trait_vertex.items.iter().filter_map(move |item_id| {
                if let Some(item) = item_index.get(item_id) {
                    matches!(item.inner, ItemEnum::AssocConst { .. })
                        .then(|| origin.make_item_vertex(item))
                } else {
                    None
                }
            }))
        }),
        _ => unreachable!("resolve_trait_edge {edge_name}"),
    }
}

pub(super) fn resolve_implemented_trait_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "trait" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let (_, trait_item) = vertex
                .as_implemented_trait()
                .expect("vertex was not an ImplementedTrait");
            Box::new(std::iter::once(origin.make_item_vertex(trait_item)))
        }),
        _ => unreachable!("resolve_implemented_trait_edge {edge_name}"),
    }
}

pub(super) fn resolve_attribute_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "content" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let attribute = vertex.as_attribute().expect("vertex was not an Attribute");
            Box::new(std::iter::once(
                origin.make_attribute_meta_item_vertex(attribute.content.clone()),
            ))
        }),
        _ => unreachable!("resolve_attribute_edge {edge_name}"),
    }
}

pub(super) fn resolve_attribute_meta_item_edge<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "argument" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let meta_item = vertex
                .as_attribute_meta_item()
                .expect("vertex was not an AttributeMetaItem");
            if let Some(arguments) = meta_item.arguments.clone() {
                Box::new(
                    arguments
                        .into_iter()
                        .map(move |argument| origin.make_attribute_meta_item_vertex(argument)),
                )
            } else {
                Box::new(std::iter::empty())
            }
        }),
        _ => unreachable!("resolve_attribute_meta_item_edge {edge_name}"),
    }
}
