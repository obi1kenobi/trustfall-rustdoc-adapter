use std::collections::BTreeSet;

use rustdoc_types::{Id, ItemEnum, Variant};
use trustfall::provider::{
    resolve_neighbors_with, ContextIterator, ContextOutcomeIterator, VertexIterator,
};

use crate::{attributes::Attribute, IndexedCrate};

use super::{origin::Origin, vertex::Vertex};

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
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "item" => resolve_neighbors_with(contexts, |vertex| {
            let origin = vertex.origin;
            let crate_vertex = vertex.as_indexed_crate().expect("vertex was not a Crate");

            let iter = crate_vertex
                .inner
                .index
                .values()
                .filter(|item| {
                    // Filter out item types that are not currently supported.
                    matches!(
                        item.inner,
                        rustdoc_types::ItemEnum::Struct(..)
                            | rustdoc_types::ItemEnum::StructField(..)
                            | rustdoc_types::ItemEnum::Enum(..)
                            | rustdoc_types::ItemEnum::Variant(..)
                            | rustdoc_types::ItemEnum::Function(..)
                            | rustdoc_types::ItemEnum::Impl(..)
                            | rustdoc_types::ItemEnum::Trait(..)
                    )
                })
                .map(move |value| origin.make_item_vertex(value));
            Box::new(iter)
        }),
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
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "impl" | "inherent_impl" => {
            let inherent_impls_only = edge_name == "inherent_impl";
            resolve_neighbors_with(contexts, move |vertex| {
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
            })
        }
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
        _ => unreachable!("resolve_function_like_edge {edge_name}"),
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

            match &item {
                Variant::Plain(_) => Box::new(std::iter::empty()),
                Variant::Tuple(fields) => {
                    Box::new(fields.iter().filter(|x| x.is_some()).map(move |field_id| {
                        origin.make_item_vertex(
                            item_index
                                .get(field_id.as_ref().unwrap())
                                .expect("missing item"),
                        )
                    }))
                }
                Variant::Struct {
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
    contexts: ContextIterator<'a, Vertex<'a>>,
    edge_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "method" => {
            resolve_neighbors_with(contexts, move |vertex| {
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
                let provided_methods: Box<dyn Iterator<Item = &Id>> =
                    if impl_vertex.provided_trait_methods.is_empty() {
                        Box::new(std::iter::empty())
                    } else {
                        let method_names: BTreeSet<&str> = impl_vertex
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
                                    let next_item = &item_index.get(item_id);
                                    if let Some(name) = next_item.and_then(|x| x.name.as_deref()) {
                                        method_names.contains(name)
                                    } else {
                                        false
                                    }
                                }))
                            } else {
                                unreachable!("found a non-trait type {trait_item:?}");
                            }
                        } else {
                            Box::new(std::iter::empty())
                        }
                    };
                Box::new(provided_methods.chain(impl_vertex.items.iter()).filter_map(
                    move |item_id| {
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
                    },
                ))
            })
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
