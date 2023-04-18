use std::{collections::BTreeSet, sync::Arc};

use rustdoc_types::{Id, ItemEnum, VariantKind};
use trustfall::{
    provider::{
        resolve_coercion_with, resolve_neighbors_with, Adapter, ContextIterator,
        ContextOutcomeIterator, EdgeParameters, ResolveEdgeInfo, ResolveInfo, Typename,
        VertexIterator,
    },
    FieldValue, Schema,
};

use crate::{attributes::Attribute, indexed_crate::IndexedCrate};

use self::{
    origin::Origin,
    vertex::{Vertex, VertexKind},
};

mod origin;
mod properties;
mod vertex;

#[non_exhaustive]
pub struct RustdocAdapter<'a> {
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
}

impl<'a> RustdocAdapter<'a> {
    pub fn new(
        current_crate: &'a IndexedCrate<'a>,
        previous_crate: Option<&'a IndexedCrate<'a>>,
    ) -> Self {
        Self {
            current_crate,
            previous_crate,
        }
    }

    pub fn schema() -> Schema {
        Schema::parse(include_str!("../rustdoc_schema.graphql")).expect("schema not valid")
    }
}

impl<'a> Adapter<'a> for RustdocAdapter<'a> {
    type Vertex = Vertex<'a>;

    fn resolve_starting_vertices(
        &self,
        edge_name: &Arc<str>,
        _parameters: &EdgeParameters,
        _resolve_info: &ResolveInfo,
    ) -> VertexIterator<'a, Self::Vertex> {
        match edge_name.as_ref() {
            "Crate" => Box::new(std::iter::once(Vertex::new_crate(
                Origin::CurrentCrate,
                self.current_crate,
            ))),
            "CrateDiff" => {
                let previous_crate = self.previous_crate.expect("no previous crate provided");
                Box::new(std::iter::once(Vertex {
                    origin: Origin::CurrentCrate,
                    kind: VertexKind::CrateDiff((self.current_crate, previous_crate)),
                }))
            }
            _ => unreachable!("resolve_starting_vertices {edge_name}"),
        }
    }

    fn resolve_property(
        &self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        property_name: &Arc<str>,
        _resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'a, Self::Vertex, FieldValue> {
        if property_name.as_ref() == "__typename" {
            Box::new(contexts.map(|ctx| match ctx.active_vertex() {
                Some(vertex) => {
                    let value = vertex.typename().into();
                    (ctx, value)
                }
                None => (ctx, FieldValue::Null),
            }))
        } else {
            match type_name.as_ref() {
                "Crate" => properties::resolve_crate_property(contexts, property_name),
                "Item" => properties::resolve_item_property(contexts, property_name),
                "ImplOwner" | "Struct" | "StructField" | "Enum" | "Variant" | "PlainVariant"
                | "TupleVariant" | "StructVariant" | "Trait" | "Function" | "Method" | "Impl"
                    if matches!(
                        property_name.as_ref(),
                        "id" | "crate_id" | "name" | "docs" | "attrs" | "visibility_limit"
                    ) =>
                {
                    // properties inherited from Item, accesssed on Item subtypes
                    properties::resolve_item_property(contexts, property_name)
                }
                "Struct" => properties::resolve_struct_property(contexts, property_name),
                "Enum" => properties::resolve_enum_property(contexts, property_name),
                "Span" => properties::resolve_span_property(contexts, property_name),
                "Path" => properties::resolve_path_property(contexts, property_name),
                "ImportablePath" => {
                    properties::resolve_importable_path_property(contexts, property_name)
                }
                "FunctionLike" | "Function" | "Method"
                    if matches!(property_name.as_ref(), "const" | "unsafe" | "async") =>
                {
                    properties::resolve_function_like_property(contexts, property_name)
                }
                "FunctionParameter" => {
                    properties::resolve_function_parameter_property(contexts, property_name)
                }
                "Impl" => properties::resolve_impl_property(contexts, property_name),
                "Attribute" => properties::resolve_attribute_property(contexts, property_name),
                "AttributeMetaItem" => {
                    properties::resolve_attribute_meta_item_property(contexts, property_name)
                }
                "Trait" => properties::resolve_trait_property(contexts, property_name),
                "ImplementedTrait" => {
                    properties::resolve_implemented_trait_property(contexts, property_name)
                }
                "RawType" | "ResolvedPathType" | "PrimitiveType"
                    if matches!(property_name.as_ref(), "name") =>
                {
                    // fields from "RawType"
                    properties::resolve_raw_type_property(contexts, property_name)
                }
                _ => unreachable!("resolve_property {type_name} {property_name}"),
            }
        }
    }

    fn resolve_neighbors(
        &self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        _resolve_info: &ResolveEdgeInfo,
    ) -> ContextOutcomeIterator<'a, Self::Vertex, VertexIterator<'a, Self::Vertex>> {
        match type_name.as_ref() {
            "CrateDiff" => match edge_name.as_ref() {
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
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "Crate" => {
                match edge_name.as_ref() {
                    "item" => resolve_neighbors_with(contexts, |vertex| {
                        let origin = vertex.origin;
                        let crate_vertex =
                            vertex.as_indexed_crate().expect("vertex was not a Crate");

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
                    _ => unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}"),
                }
            }
            "Importable" | "ImplOwner" | "Struct" | "Enum" | "Trait" | "Function"
                if matches!(edge_name.as_ref(), "importable_path" | "canonical_path") =>
            {
                match edge_name.as_ref() {
                    "canonical_path" => {
                        let current_crate = self.current_crate;
                        let previous_crate = self.previous_crate;
                        resolve_neighbors_with(contexts, move |vertex| {
                            let origin = vertex.origin;
                            let item = vertex.as_item().expect("vertex was not an Item");
                            let item_id = &item.id;

                            if let Some(path) = match origin {
                                Origin::CurrentCrate => {
                                    current_crate.inner.paths.get(item_id).map(|x| &x.path)
                                }
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
                        })
                    }
                    "importable_path" => {
                        let current_crate = self.current_crate;
                        let previous_crate = self.previous_crate;
                        resolve_neighbors_with(contexts, move |vertex| {
                            let origin = vertex.origin;
                            let item = vertex.as_item().expect("vertex was not an Item");
                            let item_id = &item.id;

                            let parent_crate = match origin {
                                Origin::CurrentCrate => current_crate,
                                Origin::PreviousCrate => {
                                    previous_crate.expect("no baseline provided")
                                }
                            };

                            Box::new(
                                parent_crate
                                    .publicly_importable_names(item_id)
                                    .into_iter()
                                    .map(move |x| origin.make_importable_path_vertex(x)),
                            )
                        })
                    }
                    _ => unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}"),
                }
            }
            "Item" | "ImplOwner" | "Struct" | "StructField" | "Enum" | "Variant"
            | "PlainVariant" | "TupleVariant" | "StructVariant" | "Trait" | "Function"
            | "Method" | "Impl"
                if matches!(edge_name.as_ref(), "span" | "attribute") =>
            {
                match edge_name.as_ref() {
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
                        Box::new(item.attrs.iter().map(move |attr| {
                            origin.make_attribute_vertex(Attribute::new(attr.as_str()))
                        }))
                    }),
                    _ => unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}"),
                }
            }
            "ImplOwner" | "Struct" | "Enum"
                if matches!(edge_name.as_ref(), "impl" | "inherent_impl") =>
            {
                let current_crate = self.current_crate;
                let previous_crate = self.previous_crate;
                let inherent_impls_only = edge_name.as_ref() == "inherent_impl";
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
            "Function" | "Method" | "FunctionLike" if matches!(edge_name.as_ref(), "parameter") => {
                resolve_neighbors_with(contexts, move |vertex| {
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
                })
            }
            "Struct" => match edge_name.as_ref() {
                "field" => {
                    let current_crate = self.current_crate;
                    let previous_crate = self.previous_crate;
                    resolve_neighbors_with(contexts, move |vertex| {
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

                        let field_ids_iter: Box<dyn Iterator<Item = &Id>> = match &struct_item.kind
                        {
                            rustdoc_types::StructKind::Unit => Box::new(std::iter::empty()),
                            rustdoc_types::StructKind::Tuple(field_ids) => {
                                Box::new(field_ids.iter().filter_map(|x| x.as_ref()))
                            }
                            rustdoc_types::StructKind::Plain { fields, .. } => {
                                Box::new(fields.iter())
                            }
                        };

                        Box::new(field_ids_iter.map(move |field_id| {
                            origin.make_item_vertex(item_index.get(field_id).expect("missing item"))
                        }))
                    })
                }
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "Variant" | "PlainVariant" | "TupleVariant" | "StructVariant" => {
                match edge_name.as_ref() {
                    "field" => {
                        let current_crate = self.current_crate;
                        let previous_crate = self.previous_crate;
                        resolve_neighbors_with(contexts, move |vertex| {
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
                                VariantKind::Tuple(fields) => Box::new(
                                    fields.iter().filter(|x| x.is_some()).map(move |field_id| {
                                        origin.make_item_vertex(
                                            item_index
                                                .get(field_id.as_ref().unwrap())
                                                .expect("missing item"),
                                        )
                                    }),
                                ),
                                VariantKind::Struct {
                                    fields,
                                    fields_stripped: _,
                                } => Box::new(fields.iter().map(move |field_id| {
                                    origin.make_item_vertex(
                                        item_index.get(field_id).expect("missing item"),
                                    )
                                })),
                            }
                        })
                    }
                    _ => {
                        unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                    }
                }
            }
            "Enum" => match edge_name.as_ref() {
                "variant" => {
                    let current_crate = self.current_crate;
                    let previous_crate = self.previous_crate;
                    resolve_neighbors_with(contexts, move |vertex| {
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
                    })
                }
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "StructField" => match edge_name.as_ref() {
                "raw_type" => resolve_neighbors_with(contexts, move |vertex| {
                    let origin = vertex.origin;
                    let field_type = vertex.as_struct_field().expect("not a StructField vertex");
                    Box::new(std::iter::once(origin.make_raw_type_vertex(field_type)))
                }),
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "Impl" => {
                match edge_name.as_ref() {
                    "method" => {
                        let current_crate = self.current_crate;
                        let previous_crate = self.previous_crate;
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

                                    let trait_path = impl_vertex.trait_.as_ref().expect(
                                        "no trait but provided_trait_methods was non-empty",
                                    );
                                    let trait_item = item_index.get(&trait_path.id);

                                    if let Some(trait_item) = trait_item {
                                        if let ItemEnum::Trait(trait_item) = &trait_item.inner {
                                            Box::new(trait_item.items.iter().filter(
                                                move |item_id| {
                                                    let next_item = &item_index.get(item_id);
                                                    if let Some(name) =
                                                        next_item.and_then(|x| x.name.as_deref())
                                                    {
                                                        method_names.contains(name)
                                                    } else {
                                                        false
                                                    }
                                                },
                                            ))
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
                    "implemented_trait" => {
                        let current_crate = self.current_crate;
                        let previous_crate = self.previous_crate;
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
                                        Origin::CurrentCrate => {
                                            &current_crate.manually_inlined_builtin_traits
                                        }
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
                        })
                    }
                    _ => {
                        unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                    }
                }
            }
            "Trait" => match edge_name.as_ref() {
                "method" => {
                    let current_crate = self.current_crate;
                    let previous_crate = self.previous_crate;
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
                    })
                }
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "ImplementedTrait" => match edge_name.as_ref() {
                "trait" => resolve_neighbors_with(contexts, move |vertex| {
                    let origin = vertex.origin;

                    let (_, trait_item) = vertex
                        .as_implemented_trait()
                        .expect("vertex was not an ImplementedTrait");
                    Box::new(std::iter::once(origin.make_item_vertex(trait_item)))
                }),
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "Attribute" => match edge_name.as_ref() {
                "content" => resolve_neighbors_with(contexts, move |vertex| {
                    let origin = vertex.origin;

                    let attribute = vertex.as_attribute().expect("vertex was not an Attribute");
                    Box::new(std::iter::once(
                        origin.make_attribute_meta_item_vertex(attribute.content.clone()),
                    ))
                }),
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "AttributeMetaItem" => {
                match edge_name.as_ref() {
                    "argument" => resolve_neighbors_with(contexts, move |vertex| {
                        let origin = vertex.origin;

                        let meta_item = vertex
                            .as_attribute_meta_item()
                            .expect("vertex was not an AttributeMetaItem");
                        if let Some(arguments) = meta_item.arguments.clone() {
                            Box::new(arguments.into_iter().map(move |argument| {
                                origin.make_attribute_meta_item_vertex(argument)
                            }))
                        } else {
                            Box::new(std::iter::empty())
                        }
                    }),
                    _ => {
                        unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                    }
                }
            }
            _ => unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}"),
        }
    }

    fn resolve_coercion(
        &self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        coerce_to_type: &Arc<str>,
        _resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'a, Self::Vertex, bool> {
        let coerce_to_type = coerce_to_type.clone();
        match type_name.as_ref() {
            "Item" | "Variant" | "FunctionLike" | "Importable" | "ImplOwner" | "RawType"
            | "ResolvedPathType" => {
                resolve_coercion_with(contexts, move |vertex| {
                    let actual_type_name = vertex.typename();

                    match coerce_to_type.as_ref() {
                        "Variant" => matches!(
                            actual_type_name,
                            "PlainVariant" | "TupleVariant" | "StructVariant"
                        ),
                        "ImplOwner" => matches!(actual_type_name, "Struct" | "Enum"),
                        "ResolvedPathType" => {
                            matches!(actual_type_name, "ResolvedPathType" | "ImplementedTrait")
                        }
                        _ => {
                            // The remaining types are final (don't have any subtypes)
                            // so we can just compare the actual type name to
                            // the type we are attempting to coerce to.
                            actual_type_name == coerce_to_type.as_ref()
                        }
                    }
                })
            }
            _ => unreachable!("resolve_coercion {type_name} {coerce_to_type}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context;

    #[test]
    fn rustdoc_json_format_version() {
        let path = "./localdata/test_data/reexport/rustdoc.json";
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to load rustdoc");

        let expected_version = rustdoc_types::FORMAT_VERSION;
        let actual_version = crate::test_util::detect_rustdoc_format_version(&content)
            .expect("unrecognized rustdoc format");

        assert_eq!(
            expected_version, actual_version,
            "Expected to find rustdoc v{expected_version} but got v{actual_version} instead.",
        );
    }
}
