use std::{collections::BTreeSet, num::NonZeroUsize};

use rustdoc_types::{
    GenericBound::TraitBound, GenericParamDefKind, Id, ItemEnum, VariantKind, WherePredicate,
};
use trustfall::provider::{
    AsVertex, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo, VertexIterator,
    resolve_neighbors_with,
};

use crate::{
    adapter::supported_item_kind,
    attributes::Attribute,
    hashtables::{HashMap, HashSet},
};

use super::{
    RustdocAdapter, optimizations,
    origin::Origin,
    receiver::Receiver,
    vertex::{Feature, Vertex},
};

pub(super) fn resolve_crate_diff_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
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

pub(super) fn resolve_crate_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "item" => optimizations::item_lookup::resolve_crate_items(adapter, contexts, resolve_info),
        "root_module" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let crate_ = vertex.as_crate().expect("vertex was not a crate!");
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let module = item_index
                .get(&crate_.root)
                .expect("crate had no root module");
            Box::new(std::iter::once(origin.make_item_vertex(module)))
        }),
        "feature" => {
            resolve_neighbors_with(contexts, move |vertex| {
                let origin = vertex.origin;

                let Some(features_lookup) = adapter.crate_at_origin(origin).features.as_ref()
                else {
                    // No feature data was loaded.
                    return Box::new(std::iter::empty());
                };

                Box::new(
                    features_lookup
                        .features
                        .values()
                        .map(move |feat| origin.make_feature_vertex(feat)),
                )
            })
        }
        "default_feature" => {
            resolve_neighbors_with(contexts, move |vertex| {
                let origin = vertex.origin;

                let Some(features_lookup) = adapter.crate_at_origin(origin).features.as_ref()
                else {
                    // No feature data was loaded.
                    return Box::new(std::iter::empty());
                };

                // If there's no `default` feature, then no features are enabled by default.
                let Some(default_feature) = features_lookup.features.get("default") else {
                    return Box::new(std::iter::empty());
                };
                let (default_enabled, _) =
                    default_feature.enables_recursive(&features_lookup.features);

                Box::new(
                    default_enabled
                        .into_values()
                        .map(move |feat| origin.make_feature_vertex(feat)),
                )
            })
        }
        "ffi_exported_function" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let export_name_index = &adapter
                .crate_at_origin(origin)
                .own_crate
                .export_name_index
                .as_ref()
                .expect("export_name_index was never constructed");

            Box::new(
                export_name_index
                    .values()
                    .filter_map(move |item| match &item.inner {
                        ItemEnum::Function(..) => {
                            debug_assert!(
                                crate::exported_name::item_export_name(item).is_some(),
                                "item was part of export_name_index but did not have \
                                an exported name: {item:?}"
                            );
                            Some(origin.make_item_vertex(item))
                        }
                        _ => None,
                    }),
            )
        }),
        _ => unreachable!("resolve_crate_edge {edge_name}"),
    }
}

pub(super) fn resolve_importable_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "canonical_path" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            let item_id = &item.id;

            if let Some(path) = adapter
                .crate_at_origin(origin)
                .own_crate
                .inner
                .paths
                .get(item_id)
                .map(|x| &x.path)
            {
                Box::new(std::iter::once(origin.make_path_vertex(path)))
            } else {
                Box::new(std::iter::empty())
            }
        }),
        "importable_path" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex.as_item().expect("vertex was not an Item");
            let item_id = &item.id;

            let parent_crate = adapter.crate_at_origin(origin);

            Box::new(
                parent_crate
                    .own_crate
                    .publicly_importable_names(item_id)
                    .into_iter()
                    .map(move |x| origin.make_importable_path_vertex(x)),
            )
        }),
        _ => unreachable!("resolve_importable_edge {edge_name}"),
    }
}

pub(super) fn resolve_item_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
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
                    .map(move |attr| origin.make_attribute_vertex(Attribute::new(attr))),
            )
        }),
        _ => unreachable!("resolve_item_edge {edge_name}"),
    }
}

pub(super) fn resolve_impl_owner_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
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

pub(super) fn resolve_function_like_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "parameter" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            Box::new(
                vertex
                    .as_function()
                    .expect("vertex was not a Function")
                    .sig
                    .inputs
                    .iter()
                    .map(move |(name, _type_)| origin.make_function_parameter_vertex(name)),
            )
        }),
        "return_value" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let return_value = origin.make_return_value_vertex(
                vertex
                    .as_function()
                    .expect("vertex was not a Function")
                    .sig
                    .output
                    .as_ref(),
            );

            Box::new(std::iter::once(return_value))
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

pub(super) fn resolve_receiver_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "receiver" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let method = vertex.as_function().expect("vertex was not a Function");

            // Check if the first parameter is a self receiver
            let receiver = method.sig.inputs.first().and_then(|(name, ty)| {
                if name == "self" {
                    Some(Receiver::new(ty))
                } else {
                    None
                }
            });

            Box::new(
                receiver
                    .into_iter()
                    .map(move |r| origin.make_receiver_vertex(r)),
            )
        }),
        _ => unreachable!("resolve_receiver_edge {edge_name}"),
    }
}

pub(super) fn resolve_generic_parameter_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    struct GenericParamCounter {
        lifetimes: NonZeroUsize,
        types_and_consts: NonZeroUsize,
    }

    match edge_name {
        "generic_parameter" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let mut counter = GenericParamCounter {
                lifetimes: NonZeroUsize::new(1).unwrap(),
                types_and_consts: NonZeroUsize::new(1).unwrap(),
            };
            Box::new(
                vertex
                    .as_generics()
                    .map(move |generics| {
                        generics.params.iter().map(move |param| {
                            let position = match param.kind {
                                GenericParamDefKind::Lifetime { .. } => {
                                    let position = counter.lifetimes;
                                    counter.lifetimes =
                                        position.checked_add(1).expect("param position overflow");
                                    Some(position)
                                }
                                GenericParamDefKind::Type { is_synthetic, .. } => {
                                    if is_synthetic {
                                        None
                                    } else {
                                        let position = counter.types_and_consts;
                                        counter.types_and_consts = position
                                            .checked_add(1)
                                            .expect("param position overflow");
                                        Some(position)
                                    }
                                }
                                GenericParamDefKind::Const { .. } => {
                                    let position = counter.types_and_consts;
                                    counter.types_and_consts =
                                        position.checked_add(1).expect("param position overflow");
                                    Some(position)
                                }
                            };
                            origin.make_generic_parameter_vertex(generics, param, position)
                        })
                    })
                    .into_iter()
                    .flatten(),
            )
        }),
        _ => unreachable!("resolve_generic_parameter_edge {edge_name}"),
    }
}

pub(super) fn resolve_module_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "item" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let module_item = vertex.as_module().expect("vertex was not a Module");

            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            Box::new(module_item.items.iter().filter_map(move |item_id| {
                item_index
                    .get(item_id)
                    .filter(|item| supported_item_kind(item))
                    .map(|item| origin.make_item_vertex(item))
            }))
        }),
        _ => unreachable!("resolve_module_edge {edge_name}"),
    }
}

pub(super) fn resolve_struct_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "field" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let struct_item = vertex.as_struct().expect("vertex was not a Struct");

            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let field_ids_iter: Box<dyn Iterator<Item = &Id>> = match &struct_item.kind {
                rustdoc_types::StructKind::Unit => Box::new(std::iter::empty()),
                rustdoc_types::StructKind::Tuple(field_ids) => {
                    Box::new(field_ids.iter().filter_map(|x| x.as_ref()))
                }
                rustdoc_types::StructKind::Plain { fields, .. } => Box::new(fields.iter()),
            };

            Box::new(field_ids_iter.enumerate().map(move |(index, field_id)| {
                origin.make_positioned_item_vertex(
                    index + 1,
                    item_index.get(field_id).expect("missing item"),
                )
            }))
        }),
        _ => unreachable!("resolve_struct_edge {edge_name}"),
    }
}

pub(super) fn resolve_variant_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "field" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item = vertex
                .as_variant()
                .expect("vertex was not a Variant")
                .variant();
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            match &item.kind {
                VariantKind::Plain => Box::new(std::iter::empty()),
                VariantKind::Tuple(fields) => {
                    Box::new(fields.iter().filter(|x| x.is_some()).enumerate().map(
                        move |(index, field_id)| {
                            origin.make_positioned_item_vertex(
                                index + 1,
                                item_index
                                    .get(field_id.as_ref().unwrap())
                                    .expect("missing item"),
                            )
                        },
                    ))
                }
                VariantKind::Struct {
                    fields,
                    has_stripped_fields: _,
                } => Box::new(fields.iter().enumerate().map(move |(index, field_id)| {
                    origin.make_positioned_item_vertex(
                        index + 1,
                        item_index.get(field_id).expect("missing item"),
                    )
                })),
            }
        }),
        "discriminant" => resolve_neighbors_with(contexts, move |vertex: &'_ Vertex<'a>| {
            let origin = vertex.origin;
            let enum_var = vertex.as_variant().expect("vertex was not a Variant");
            let maybe_discriminant = enum_var.discriminant();

            Box::new(
                maybe_discriminant
                    .into_iter()
                    .map(move |discriminant| origin.make_discriminant_vertex(discriminant)),
            )
        }),
        _ => unreachable!("resolve_variant_edge {edge_name}"),
    }
}

pub(super) fn resolve_enum_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "variant" => {
            optimizations::variant_lookup::resolve_enum_variant(adapter, contexts, resolve_info)
        }
        _ => unreachable!("resolve_enum_edge {edge_name}"),
    }
}

pub(super) fn resolve_union_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "field" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let union_item = vertex.as_union().expect("vertex was not an Union");

            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            Box::new(
                union_item
                    .fields
                    .iter()
                    .enumerate()
                    .map(move |(index, field_id)| {
                        origin.make_positioned_item_vertex(
                            index + 1,
                            item_index.get(field_id).expect("missing item"),
                        )
                    }),
            )
        }),
        _ => unreachable!("resolve_union_edge {edge_name}"),
    }
}

pub(super) fn resolve_struct_field_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "raw_type" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let field_type = vertex.as_struct_field().expect("not a StructField vertex");
            Box::new(std::iter::once(origin.make_raw_type_vertex(field_type)))
        }),
        _ => unreachable!("resolve_struct_field_edge {edge_name}"),
    }
}

pub(super) fn resolve_impl_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "method" => {
            optimizations::method_lookup::resolve_impl_methods(adapter, contexts, resolve_info)
        }
        "implemented_trait" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

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
                    adapter
                        .crate_at_origin(origin)
                        .own_crate
                        .manually_inlined_builtin_traits
                        .get(&path.id)
                });

                Box::new(std::iter::once(
                    origin.make_implemented_trait_vertex(path, None, found_item),
                ))
            } else {
                Box::new(std::iter::empty())
            }
        }),
        "associated_constant" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

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

pub(super) fn resolve_trait_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "supertrait" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let trait_vertex = vertex.as_trait().expect("not a Trait vertex");
            Box::new(trait_vertex.bounds.iter().filter_map(move |bound| {
                if let TraitBound { trait_, .. } = &bound {
                    // When the implemented trait is from the same crate
                    // as its definition, the trait is expected to be present
                    // in `item_index`. Otherwise, the
                    // `rustdoc_types::Trait` is not in this rustdoc,
                    // even if the trait is part of Rust `core` or `std`.
                    // As a temporary workaround, some common
                    // Rust built-in traits are manually "inlined"
                    // with items stored in `manually_inlined_builtin_traits`.
                    let found_item = item_index.get(&trait_.id).or_else(|| {
                        adapter
                            .crate_at_origin(origin)
                            .own_crate
                            .manually_inlined_builtin_traits
                            .get(&trait_.id)
                    });

                    // TODO: Remove this once rust-analyzer stops falsely inferring the type of
                    //       `bound` as `GenericBound` when in fact it's `&GenericBound`.
                    //       It shows a phantom compile error unless we add `&` before `bound`.
                    #[allow(clippy::needless_borrow)]
                    let trait_bound: Option<&rustdoc_types::GenericBound> = Some(&bound);

                    Some(origin.make_implemented_trait_vertex(trait_, trait_bound, found_item))
                } else {
                    None
                }
            }))
        }),
        "method" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

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
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

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
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

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

pub(super) fn resolve_implemented_trait_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "trait" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let impld_trait = vertex
                .as_implemented_trait()
                .expect("vertex was not an ImplementedTrait");

            Box::new(
                impld_trait
                    .resolved_item
                    .into_iter()
                    .map(move |item| origin.make_item_vertex(item)),
            )
        }),
        _ => unreachable!("resolve_implemented_trait_edge {edge_name}"),
    }
}

pub(super) fn resolve_attribute_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "content" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let attribute = vertex.as_attribute().expect("vertex was not an Attribute");
            Box::new(std::iter::once(
                origin.make_attribute_meta_item_vertex(attribute.content()),
            ))
        }),
        _ => unreachable!("resolve_attribute_edge {edge_name}"),
    }
}

pub(super) fn resolve_attribute_meta_item_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "argument" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let meta_item = vertex
                .as_attribute_meta_item()
                .expect("vertex was not an AttributeMetaItem");

            Box::new(
                meta_item
                    .arguments()
                    .into_iter()
                    .map(move |argument| origin.make_attribute_meta_item_vertex(argument)),
            )
        }),
        _ => unreachable!("resolve_attribute_meta_item_edge {edge_name}"),
    }
}

pub(super) fn resolve_derive_proc_macro_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "helper_attribute" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;

            let proc_macro = vertex
                .as_proc_macro()
                .expect("vertex was not a DeriveProcMacro");
            Box::new(
                proc_macro
                    .helpers
                    .iter()
                    .map(move |helper| origin.make_derive_helper_attr_vertex(helper)),
            )
        }),
        _ => unreachable!("resolve_derive_proc_macro_edge {edge_name}"),
    }
}

pub(super) fn resolve_generic_type_parameter_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "type_bound" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let (generics, param): (
                &'a rustdoc_types::Generics,
                &'a rustdoc_types::GenericParamDef,
            ) = vertex
                .as_generic_parameter()
                .expect("vertex was not a GenericTypeParameter");

            // Bounds directly applied to the generic, like `<T: Clone>`.
            let explicit_bounds = match &param.kind {
                rustdoc_types::GenericParamDefKind::Type { bounds, .. } => bounds.as_slice(),
                _ => unreachable!("vertex was not a GenericTypeParameter: {vertex:?}"),
            };

            // Lift `where` bounds that could have been written as bounds on the generic.
            // For example: `where T: Clone` is the same as `<T: Clone>` so we want to extract it.
            // For cases like `where T: Iterator, T::Item: Clone`, we only extract `<T: Iterator>`.
            // We leave more complex cases alone, like `where Arc<T>: Clone`
            // or `where for<'a> &'a: Iterator`.
            let where_bounds = generics.where_predicates.iter().filter_map(move |predicate| {
                match predicate {
                    WherePredicate::BoundPredicate { type_, bounds, generic_params } => {
                        if !generic_params.is_empty() {
                            // `generic_params` is only used for HRTBs,
                            // which can't be represented as bounds on the generic itself.
                            return None;
                        }

                        if !matches!(type_, rustdoc_types::Type::Generic(name) if name == &param.name) {
                            // This bound is not directly on the generic we're looking at.
                            // For example, it might be `where T::Item: Clone`,
                            // or it might be on a different generic parameter, like `U: Clone`.
                            return None;
                        }

                        Some(bounds.as_slice())
                    }
                    WherePredicate::LifetimePredicate { .. } | WherePredicate::EqPredicate { .. } => {
                        // Neither of these cases can be written as a bound on a generic parameter.
                        None
                    }
                }
            }).flatten();

            Box::new(
                explicit_bounds
                    .iter()
                    .chain(where_bounds)
                    .filter_map(move |bound| {
                        if let TraitBound { trait_, .. } = &bound {
                            // When the implemented trait is from the same crate
                            // as its definition, the trait is expected to be present
                            // in `item_index`. Otherwise, the
                            // `rustdoc_types::Trait` is not in this rustdoc,
                            // even if the trait is part of Rust `core` or `std`.
                            // As a temporary workaround, some common
                            // Rust built-in traits are manually "inlined"
                            // with items stored in `manually_inlined_builtin_traits`.
                            let found_item = item_index.get(&trait_.id).or_else(|| {
                                adapter
                                    .crate_at_origin(origin)
                                    .own_crate
                                    .manually_inlined_builtin_traits
                                    .get(&trait_.id)
                            });

                            Some(origin.make_implemented_trait_vertex(
                                trait_,
                                Some(bound),
                                found_item,
                            ))
                        } else {
                            None
                        }
                    }),
            )
        }),
        _ => unreachable!("resolve_generic_type_parameter_edge {edge_name}"),
    }
}

pub(super) fn resolve_feature_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    edge_name: &str,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    match edge_name {
        "directly_enables" => resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let feature: &Feature<'_> = vertex.as_feature().expect("vertex was not a Feature");

            let features_lookup = adapter
                .crate_at_origin(origin)
                .features
                .as_ref()
                .expect("no feature data was loaded");

            Box::new(
                feature
                    .inner
                    .enables_features
                    .iter()
                    .copied()
                    .filter_map(move |key| {
                        features_lookup
                            .features
                            .get(key)
                            .map(|f| origin.make_feature_vertex(f))
                    }),
            )
        }),
        _ => unreachable!("resolve_feature_edge {edge_name}"),
    }
}

pub(super) fn resolve_requires_target_feature_edge<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    adapter: &'a RustdocAdapter<'a>,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    resolve_neighbors_with(contexts, move |vertex| {
        let origin = vertex.origin;
        let item = vertex.as_item().expect("vertex was not an Item");

        let features_lookup = &adapter.crate_at_origin(origin).own_crate.target_features;

        let enabled_features = item
            .attrs
            .iter()
            .filter_map(|attr| match attr {
                rustdoc_types::Attribute::TargetFeature { enable } => Some(enable),
                _ => None,
            })
            .flatten()
            .map(|feature_name| {
                let feature_data = features_lookup.get(feature_name.as_str()).copied();
                (feature_name.as_str(), feature_data)
            });

        let resolver = TargetFeatureResolver::new(enabled_features, features_lookup);
        Box::new(<TargetFeatureResolver<'_, _> as Iterator>::map(
            resolver,
            move |(name, feature_data, explicit)| {
                origin.make_required_target_feature(name, feature_data, explicit)
            },
        ))
    })
}

struct TargetFeatureResolver<'a, T> {
    enabled_features: T,
    features_lookup: &'a HashMap<&'a str, &'a rustdoc_types::TargetFeature>,
    produced_features: HashSet<&'a str>,
    implied_features: BTreeSet<&'a str>, // we return items from this set, we need determinism
}

impl<'a, T> TargetFeatureResolver<'a, T> {
    fn new(
        enabled_features: T,
        features_lookup: &'a HashMap<&'a str, &'a rustdoc_types::TargetFeature>,
    ) -> Self {
        Self {
            enabled_features,
            features_lookup,
            produced_features: Default::default(),
            implied_features: Default::default(),
        }
    }
}

impl<'a, T> Iterator for TargetFeatureResolver<'a, T>
where
    T: Iterator<Item = (&'a str, Option<&'a rustdoc_types::TargetFeature>)>,
{
    type Item = (&'a str, Option<&'a rustdoc_types::TargetFeature>, bool);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((feature_name, feature_data)) = self.enabled_features.next() {
            if self.produced_features.insert(feature_name) {
                // We have not already produced this feature.
                // Record its unproduced implied features and produce it.
                if let Some(feature_data) = feature_data {
                    self.implied_features.extend(
                        feature_data
                            .implies_features
                            .iter()
                            .map(String::as_str)
                            .filter(|feat| !self.produced_features.contains(feat)),
                    );
                }

                return Some((feature_name, feature_data, true));
            }
        }

        // We've run out of explicitly enabled features.
        // Go through implicitly enabled ones.
        while let Some(feature_name) = self.implied_features.pop_first() {
            if self.produced_features.insert(feature_name) {
                // We have not already produced this feature.
                // Record its unproduced implied features and produce it.
                let feature_data = self.features_lookup.get(feature_name).copied();
                if let Some(feature_data) = feature_data {
                    self.implied_features.extend(
                        feature_data
                            .implies_features
                            .iter()
                            .map(String::as_str)
                            .filter(|feat| !self.produced_features.contains(feat)),
                    );
                }

                return Some((feature_name, feature_data, false));
            }
        }

        None
    }
}
