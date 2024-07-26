use std::sync::Arc;

use rustdoc_types::Item;
use trustfall::{
    provider::{
        resolve_coercion_with, Adapter, AsVertex, ContextIterator, ContextOutcomeIterator,
        EdgeParameters, ResolveEdgeInfo, ResolveInfo, Typename, VertexIterator,
    },
    FieldValue, Schema,
};

use crate::indexed_crate::IndexedCrate;

use self::{
    origin::Origin,
    vertex::{Vertex, VertexKind},
};

mod edges;
mod enum_variant;
mod optimizations;
mod origin;
mod properties;
mod vertex;

#[cfg(test)]
mod tests;

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

    fn resolve_property<V: AsVertex<Self::Vertex> + 'a>(
        &self,
        contexts: ContextIterator<'a, V>,
        type_name: &Arc<str>,
        property_name: &Arc<str>,
        _resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'a, V, FieldValue> {
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
                | "TupleVariant" | "StructVariant" | "Union" | "Trait" | "Function" | "Method"
                | "Impl" | "GlobalValue" | "Constant" | "Static" | "AssociatedType"
                | "AssociatedConstant" | "Module"
                    if matches!(
                        property_name.as_ref(),
                        "id" | "crate_id"
                            | "name"
                            | "docs"
                            | "attrs"
                            | "doc_hidden"
                            | "deprecated"
                            | "public_api_eligible"
                            | "visibility_limit"
                    ) =>
                {
                    // properties inherited from Item, accesssed on Item subtypes
                    properties::resolve_item_property(contexts, property_name)
                }
                "Module" => properties::resolve_module_property(contexts, property_name),
                "Struct" => properties::resolve_struct_property(contexts, property_name),
                "Enum" => properties::resolve_enum_property(contexts, property_name),
                "Union" => properties::resolve_union_property(contexts, property_name),
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
                "Function" => properties::resolve_function_property(contexts, property_name),
                "FunctionParameter" => {
                    properties::resolve_function_parameter_property(contexts, property_name)
                }
                "FunctionAbi" => properties::resolve_function_abi_property(contexts, property_name),
                "Impl" => properties::resolve_impl_property(contexts, property_name),
                "Attribute" => properties::resolve_attribute_property(contexts, property_name),
                "AttributeMetaItem" => {
                    properties::resolve_attribute_meta_item_property(contexts, property_name)
                }
                "Trait" => properties::resolve_trait_property(contexts, property_name),
                "ImplementedTrait" => {
                    properties::resolve_implemented_trait_property(contexts, property_name)
                }
                "Static" => properties::resolve_static_property(contexts, property_name),
                "RawType" | "ResolvedPathType" if matches!(property_name.as_ref(), "name") => {
                    // fields from "RawType"
                    properties::resolve_raw_type_property(contexts, property_name)
                }
                "AssociatedType" => {
                    properties::resolve_associated_type_property(contexts, property_name)
                }
                "AssociatedConstant" => {
                    properties::resolve_associated_constant_property(contexts, property_name)
                }
                "Constant" => properties::resolve_constant_property(contexts, property_name),
                "Discriminant" => {
                    properties::resolve_discriminant_property(contexts, property_name)
                }
                _ => unreachable!("resolve_property {type_name} {property_name}"),
            }
        }
    }

    fn resolve_neighbors<V: AsVertex<Self::Vertex> + 'a>(
        &self,
        contexts: ContextIterator<'a, V>,
        type_name: &Arc<str>,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        resolve_info: &ResolveEdgeInfo,
    ) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Self::Vertex>> {
        match type_name.as_ref() {
            "CrateDiff" => edges::resolve_crate_diff_edge(contexts, edge_name),
            "Crate" => edges::resolve_crate_edge(self, contexts, edge_name, resolve_info),
            "Importable" | "ImplOwner" | "Struct" | "Enum" | "Union" | "Trait" | "Function"
            | "GlobalValue" | "Constant" | "Static" | "Module"
                if matches!(edge_name.as_ref(), "importable_path" | "canonical_path") =>
            {
                edges::resolve_importable_edge(
                    contexts,
                    edge_name,
                    self.current_crate,
                    self.previous_crate,
                )
            }
            "Item" | "ImplOwner" | "Struct" | "StructField" | "Enum" | "Variant"
            | "PlainVariant" | "TupleVariant" | "Union" | "StructVariant" | "Trait"
            | "Function" | "Method" | "Impl" | "GlobalValue" | "Constant" | "Static"
            | "AssociatedType" | "AssociatedConstant" | "Module"
                if matches!(edge_name.as_ref(), "span" | "attribute") =>
            {
                edges::resolve_item_edge(contexts, edge_name)
            }
            "ImplOwner" | "Struct" | "Enum" | "Union"
                if matches!(edge_name.as_ref(), "impl" | "inherent_impl") =>
            {
                edges::resolve_impl_owner_edge(self, contexts, edge_name, resolve_info)
            }
            "Function" | "Method" | "FunctionLike"
                if matches!(edge_name.as_ref(), "parameter" | "abi") =>
            {
                edges::resolve_function_like_edge(contexts, edge_name)
            }
            "Module" => edges::resolve_module_edge(
                contexts,
                edge_name,
                self.current_crate,
                self.previous_crate,
            ),
            "Struct" => edges::resolve_struct_edge(
                contexts,
                edge_name,
                self.current_crate,
                self.previous_crate,
            ),
            "Variant" | "PlainVariant" | "TupleVariant" | "StructVariant" => {
                edges::resolve_variant_edge(
                    contexts,
                    edge_name,
                    self.current_crate,
                    self.previous_crate,
                )
            }
            "Enum" => edges::resolve_enum_edge(
                contexts,
                edge_name,
                self.current_crate,
                self.previous_crate,
            ),
            "Union" => edges::resolve_union_edge(
                contexts,
                edge_name,
                self.current_crate,
                self.previous_crate,
            ),
            "StructField" => edges::resolve_struct_field_edge(contexts, edge_name),
            "Impl" => edges::resolve_impl_edge(self, contexts, edge_name, resolve_info),
            "Trait" => edges::resolve_trait_edge(
                contexts,
                edge_name,
                self.current_crate,
                self.previous_crate,
            ),
            "ImplementedTrait" => edges::resolve_implemented_trait_edge(contexts, edge_name),
            "Attribute" => edges::resolve_attribute_edge(contexts, edge_name),
            "AttributeMetaItem" => edges::resolve_attribute_meta_item_edge(contexts, edge_name),
            _ => unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}"),
        }
    }

    fn resolve_coercion<V: AsVertex<Self::Vertex> + 'a>(
        &self,
        contexts: ContextIterator<'a, V>,
        type_name: &Arc<str>,
        coerce_to_type: &Arc<str>,
        _resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'a, V, bool> {
        let coerce_to_type = coerce_to_type.clone();
        match type_name.as_ref() {
            "Item" | "Variant" | "FunctionLike" | "Importable" | "ImplOwner" | "RawType"
            | "GlobalValue" => {
                resolve_coercion_with(contexts, move |vertex| {
                    let actual_type_name = vertex.typename();

                    match coerce_to_type.as_ref() {
                        "Variant" => matches!(
                            actual_type_name,
                            "PlainVariant" | "TupleVariant" | "StructVariant"
                        ),
                        "ImplOwner" => matches!(actual_type_name, "Struct" | "Enum" | "Union"),
                        "GlobalValue" => matches!(actual_type_name, "Constant" | "Static",),
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

pub(crate) fn supported_item_kind(item: &Item) -> bool {
    matches!(
        item.inner,
        rustdoc_types::ItemEnum::Struct(..)
            | rustdoc_types::ItemEnum::StructField(..)
            | rustdoc_types::ItemEnum::Enum(..)
            | rustdoc_types::ItemEnum::Variant(..)
            | rustdoc_types::ItemEnum::Union(..)
            | rustdoc_types::ItemEnum::Function(..)
            | rustdoc_types::ItemEnum::Impl(..)
            | rustdoc_types::ItemEnum::Trait(..)
            | rustdoc_types::ItemEnum::Constant(..)
            | rustdoc_types::ItemEnum::Static(..)
            | rustdoc_types::ItemEnum::AssocType { .. }
            | rustdoc_types::ItemEnum::Module { .. }
    )
}
