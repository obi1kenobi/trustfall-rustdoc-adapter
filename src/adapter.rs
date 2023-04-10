use std::{collections::BTreeSet, rc::Rc, sync::Arc};

use rustdoc_types::{
    Crate, Enum, Function, Id, Impl, Item, ItemEnum, Path, Span, Struct, Trait, Type, Variant,
};
use trustfall::{
    provider::{
        accessor_property, field_property, resolve_coercion_with, resolve_neighbors_with,
        resolve_property_with, Adapter, ContextIterator, ContextOutcomeIterator, EdgeParameters,
        ResolveEdgeInfo, ResolveInfo, Typename, VertexIterator,
    },
    FieldValue, Schema,
};

use crate::attributes::{Attribute, AttributeMetaItem};
use crate::indexed_crate::IndexedCrate;

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
        Schema::parse(include_str!("rustdoc_schema.graphql")).expect("schema not valid")
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub enum Origin {
    CurrentCrate,
    PreviousCrate,
}

impl Origin {
    fn make_item_vertex<'a>(&self, item: &'a Item) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: item.into(),
        }
    }

    fn make_span_vertex<'a>(&self, span: &'a Span) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: span.into(),
        }
    }

    fn make_path_vertex<'a>(&self, path: &'a [String]) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Path(path),
        }
    }

    fn make_importable_path_vertex<'a>(&self, importable_path: Vec<&'a str>) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::ImportablePath(importable_path),
        }
    }

    fn make_raw_type_vertex<'a>(&self, raw_type: &'a rustdoc_types::Type) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::RawType(raw_type),
        }
    }

    fn make_attribute_vertex<'a>(&self, attr: Attribute<'a>) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Attribute(attr),
        }
    }

    fn make_attribute_meta_item_vertex<'a>(
        &self,
        meta_item: Rc<AttributeMetaItem<'a>>,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::AttributeMetaItem(meta_item),
        }
    }

    fn make_implemented_trait_vertex<'a>(
        &self,
        path: &'a rustdoc_types::Path,
        trait_def: &'a Item,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::ImplementedTrait(path, trait_def),
        }
    }

    fn make_function_parameter_vertex<'a>(&self, name: &'a str) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::FunctionParameter(name),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Vertex<'a> {
    origin: Origin,
    kind: VertexKind<'a>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum VertexKind<'a> {
    CrateDiff((&'a IndexedCrate<'a>, &'a IndexedCrate<'a>)),
    Crate(&'a IndexedCrate<'a>),
    Item(&'a Item),
    Span(&'a Span),
    Path(&'a [String]),
    ImportablePath(Vec<&'a str>),
    RawType(&'a Type),
    Attribute(Attribute<'a>),
    AttributeMetaItem(Rc<AttributeMetaItem<'a>>),
    ImplementedTrait(&'a Path, &'a Item),
    FunctionParameter(&'a str),
}

impl<'a> Typename for Vertex<'a> {
    /// The name of the actual runtime type of this vertex,
    /// intended to fulfill resolution requests for the __typename property.
    #[inline]
    fn typename(&self) -> &'static str {
        match self.kind {
            VertexKind::Item(item) => match &item.inner {
                rustdoc_types::ItemEnum::Struct(..) => "Struct",
                rustdoc_types::ItemEnum::Enum(..) => "Enum",
                rustdoc_types::ItemEnum::Function(..) => "Function",
                rustdoc_types::ItemEnum::Variant(Variant::Plain(..)) => "PlainVariant",
                rustdoc_types::ItemEnum::Variant(Variant::Tuple(..)) => "TupleVariant",
                rustdoc_types::ItemEnum::Variant(Variant::Struct { .. }) => "StructVariant",
                rustdoc_types::ItemEnum::StructField(..) => "StructField",
                rustdoc_types::ItemEnum::Impl(..) => "Impl",
                rustdoc_types::ItemEnum::Trait(..) => "Trait",
                _ => unreachable!("unexpected item.inner for item: {item:?}"),
            },
            VertexKind::Span(..) => "Span",
            VertexKind::Path(..) => "Path",
            VertexKind::ImportablePath(..) => "ImportablePath",
            VertexKind::Crate(..) => "Crate",
            VertexKind::CrateDiff(..) => "CrateDiff",
            VertexKind::Attribute(..) => "Attribute",
            VertexKind::AttributeMetaItem(..) => "AttributeMetaItem",
            VertexKind::ImplementedTrait(..) => "ImplementedTrait",
            VertexKind::RawType(ty) => match ty {
                rustdoc_types::Type::ResolvedPath { .. } => "ResolvedPathType",
                rustdoc_types::Type::Primitive(..) => "PrimitiveType",
                _ => "OtherType",
            },
            VertexKind::FunctionParameter(..) => "FunctionParameter",
        }
    }
}

#[allow(dead_code)]
impl<'a> Vertex<'a> {
    fn new_crate(origin: Origin, crate_: &'a IndexedCrate<'a>) -> Self {
        Self {
            origin,
            kind: VertexKind::Crate(crate_),
        }
    }

    fn as_crate_diff(&self) -> Option<(&'a IndexedCrate<'a>, &'a IndexedCrate<'a>)> {
        match &self.kind {
            VertexKind::CrateDiff(tuple) => Some(*tuple),
            _ => None,
        }
    }

    fn as_indexed_crate(&self) -> Option<&'a IndexedCrate<'a>> {
        match self.kind {
            VertexKind::Crate(c) => Some(c),
            _ => None,
        }
    }

    fn as_crate(&self) -> Option<&'a Crate> {
        self.as_indexed_crate().map(|c| c.inner)
    }

    fn as_item(&self) -> Option<&'a Item> {
        match self.kind {
            VertexKind::Item(item) => Some(item),
            _ => None,
        }
    }

    fn as_struct(&self) -> Option<&'a Struct> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Struct(s) => Some(s),
            _ => None,
        })
    }

    fn as_struct_field(&self) -> Option<&'a Type> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::StructField(s) => Some(s),
            _ => None,
        })
    }

    fn as_span(&self) -> Option<&'a Span> {
        match self.kind {
            VertexKind::Span(s) => Some(s),
            _ => None,
        }
    }

    fn as_enum(&self) -> Option<&'a Enum> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Enum(e) => Some(e),
            _ => None,
        })
    }

    fn as_trait(&self) -> Option<&'a Trait> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Trait(t) => Some(t),
            _ => None,
        })
    }

    fn as_variant(&self) -> Option<&'a Variant> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Variant(v) => Some(v),
            _ => None,
        })
    }

    fn as_path(&self) -> Option<&'a [String]> {
        match &self.kind {
            VertexKind::Path(path) => Some(*path),
            _ => None,
        }
    }

    fn as_importable_path(&self) -> Option<&'_ Vec<&'a str>> {
        match &self.kind {
            VertexKind::ImportablePath(path) => Some(path),
            _ => None,
        }
    }

    fn as_function(&self) -> Option<&'a Function> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Function(func) => Some(func),
            _ => None,
        })
    }

    fn as_function_parameter(&self) -> Option<&'a str> {
        match &self.kind {
            VertexKind::FunctionParameter(name) => Some(name),
            _ => None,
        }
    }

    fn as_impl(&self) -> Option<&'a Impl> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Impl(x) => Some(x),
            _ => None,
        })
    }

    fn as_attribute(&self) -> Option<&'_ Attribute<'a>> {
        match &self.kind {
            VertexKind::Attribute(attr) => Some(attr),
            _ => None,
        }
    }

    fn as_attribute_meta_item(&self) -> Option<&'_ AttributeMetaItem<'a>> {
        match &self.kind {
            VertexKind::AttributeMetaItem(meta_item) => Some(meta_item),
            _ => None,
        }
    }

    fn as_raw_type(&self) -> Option<&'a rustdoc_types::Type> {
        match &self.kind {
            VertexKind::RawType(ty) => Some(*ty),
            _ => None,
        }
    }

    fn as_implemented_trait(&self) -> Option<(&'a rustdoc_types::Path, &'a Item)> {
        match &self.kind {
            VertexKind::ImplementedTrait(path, trait_item) => Some((*path, *trait_item)),
            _ => None,
        }
    }
}

impl<'a> From<&'a Item> for VertexKind<'a> {
    fn from(item: &'a Item) -> Self {
        Self::Item(item)
    }
}

impl<'a> From<&'a IndexedCrate<'a>> for VertexKind<'a> {
    fn from(c: &'a IndexedCrate<'a>) -> Self {
        Self::Crate(c)
    }
}

impl<'a> From<&'a Span> for VertexKind<'a> {
    fn from(s: &'a Span) -> Self {
        Self::Span(s)
    }
}

fn resolve_crate_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "root" => resolve_property_with(
            contexts,
            field_property!(as_crate, root, { root.0.clone().into() }),
        ),
        "crate_version" => {
            resolve_property_with(contexts, field_property!(as_crate, crate_version))
        }
        "includes_private" => {
            resolve_property_with(contexts, field_property!(as_crate, includes_private))
        }
        "format_version" => {
            resolve_property_with(contexts, field_property!(as_crate, format_version))
        }
        _ => unreachable!("Crate property {property_name}"),
    }
}

fn resolve_item_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "id" => resolve_property_with(
            contexts,
            field_property!(as_item, id, { id.0.clone().into() }),
        ),
        "crate_id" => resolve_property_with(contexts, field_property!(as_item, crate_id)),
        "name" => resolve_property_with(contexts, field_property!(as_item, name)),
        "docs" => resolve_property_with(contexts, field_property!(as_item, docs)),
        "attrs" => resolve_property_with(contexts, field_property!(as_item, attrs)),
        "visibility_limit" => resolve_property_with(contexts, |vertex| {
            let item = vertex.as_item().expect("not an item");
            match &item.visibility {
                rustdoc_types::Visibility::Public => "public".into(),
                rustdoc_types::Visibility::Default => "default".into(),
                rustdoc_types::Visibility::Crate => "crate".into(),
                rustdoc_types::Visibility::Restricted { parent: _, path } => {
                    format!("restricted ({path})").into()
                }
            }
        }),
        _ => unreachable!("Item property {property_name}"),
    }
}

fn resolve_struct_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "struct_type" => resolve_property_with(contexts, |vertex| {
            let struct_vertex = vertex.as_struct().expect("not a struct");
            match struct_vertex.kind {
                rustdoc_types::StructKind::Plain { .. } => "plain",
                rustdoc_types::StructKind::Tuple(..) => "tuple",
                rustdoc_types::StructKind::Unit => "unit",
            }
            .into()
        }),
        "fields_stripped" => resolve_property_with(contexts, |vertex| {
            let struct_vertex = vertex.as_struct().expect("not a struct");
            match struct_vertex.kind {
                rustdoc_types::StructKind::Plain {
                    fields_stripped, ..
                } => fields_stripped.into(),
                _ => FieldValue::Null,
            }
        }),
        _ => unreachable!("Struct property {property_name}"),
    }
}

fn resolve_span_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "filename" => resolve_property_with(
            contexts,
            field_property!(as_span, filename, {
                filename.to_str().expect("non-representable path").into()
            }),
        ),
        "begin_line" => resolve_property_with(
            contexts,
            field_property!(as_span, begin, { (begin.0 as u64).into() }),
        ),
        "begin_column" => resolve_property_with(
            contexts,
            field_property!(as_span, begin, { (begin.1 as u64).into() }),
        ),
        "end_line" => resolve_property_with(
            contexts,
            field_property!(as_span, end, { (end.0 as u64).into() }),
        ),
        "end_column" => resolve_property_with(
            contexts,
            field_property!(as_span, end, { (end.1 as u64).into() }),
        ),
        _ => unreachable!("Span property {property_name}"),
    }
}

fn resolve_enum_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "variants_stripped" => {
            resolve_property_with(contexts, field_property!(as_enum, variants_stripped))
        }
        _ => unreachable!("Enum property {property_name}"),
    }
}

fn resolve_path_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "path" => resolve_property_with(contexts, |vertex| {
            vertex.as_path().expect("not a path").into()
        }),
        _ => unreachable!("Path property {property_name}"),
    }
}

fn resolve_importable_path_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "path" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_importable_path()
                .expect("not an importable path")
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .into()
        }),
        "visibility_limit" => resolve_property_with(contexts, |_| "public".into()),
        _ => unreachable!("ImportablePath property {property_name}"),
    }
}

fn resolve_function_like_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "const" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.const_.into() }),
        ),
        "async" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.async_.into() }),
        ),
        "unsafe" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.unsafe_.into() }),
        ),
        _ => unreachable!("FunctionLike property {property_name}"),
    }
}

fn resolve_function_parameter_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_function_parameter()
                .expect("not a function parameter")
                .into()
        }),
        _ => unreachable!("FunctionParameter property {property_name}"),
    }
}

fn resolve_impl_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_impl, is_unsafe)),
        "negative" => resolve_property_with(contexts, field_property!(as_impl, negative)),
        "synthetic" => resolve_property_with(contexts, field_property!(as_impl, synthetic)),
        _ => unreachable!("Impl property {property_name}"),
    }
}

fn resolve_attribute_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "raw_attribute" => {
            resolve_property_with(contexts, accessor_property!(as_attribute, raw_attribute))
        }
        "is_inner" => resolve_property_with(contexts, field_property!(as_attribute, is_inner)),
        _ => unreachable!("Attribute property {property_name}"),
    }
}

fn resolve_attribute_meta_item_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "raw_item" => {
            resolve_property_with(contexts, field_property!(as_attribute_meta_item, raw_item))
        }
        "base" => resolve_property_with(contexts, field_property!(as_attribute_meta_item, base)),
        "assigned_item" => resolve_property_with(
            contexts,
            field_property!(as_attribute_meta_item, assigned_item),
        ),
        _ => unreachable!("AttributeMetaItem property {property_name}"),
    }
}

fn resolve_raw_type_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            let type_vertex = vertex.as_raw_type().expect("not a RawType");
            match type_vertex {
                rustdoc_types::Type::ResolvedPath(path) => path.name.clone().into(),
                rustdoc_types::Type::Primitive(name) => name.clone().into(),
                _ => unreachable!("unexpected RawType vertex content: {type_vertex:?}"),
            }
        }),
        _ => unreachable!("RawType property {property_name}"),
    }
}

fn resolve_trait_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_trait, is_unsafe)),
        _ => unreachable!("Trait property {property_name}"),
    }
}

fn resolve_implemented_trait_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            let (path, _) = vertex
                .as_implemented_trait()
                .expect("not an ImplementedTrait");
            path.name.clone().into()
        }),
        _ => unreachable!("Trait property {property_name}"),
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
                "Crate" => resolve_crate_property(contexts, property_name),
                "Item" => resolve_item_property(contexts, property_name),
                "ImplOwner" | "Struct" | "StructField" | "Enum" | "Variant" | "PlainVariant"
                | "TupleVariant" | "StructVariant" | "Trait" | "Function" | "Method" | "Impl"
                    if matches!(
                        property_name.as_ref(),
                        "id" | "crate_id" | "name" | "docs" | "attrs" | "visibility_limit"
                    ) =>
                {
                    // properties inherited from Item, accesssed on Item subtypes
                    resolve_item_property(contexts, property_name)
                }
                "Struct" => resolve_struct_property(contexts, property_name),
                "Enum" => resolve_enum_property(contexts, property_name),
                "Span" => resolve_span_property(contexts, property_name),
                "Path" => resolve_path_property(contexts, property_name),
                "ImportablePath" => resolve_importable_path_property(contexts, property_name),
                "FunctionLike" | "Function" | "Method"
                    if matches!(property_name.as_ref(), "const" | "unsafe" | "async") =>
                {
                    resolve_function_like_property(contexts, property_name)
                }
                "FunctionParameter" => resolve_function_parameter_property(contexts, property_name),
                "Impl" => resolve_impl_property(contexts, property_name),
                "Attribute" => resolve_attribute_property(contexts, property_name),
                "AttributeMetaItem" => {
                    resolve_attribute_meta_item_property(contexts, property_name)
                }
                "Trait" => resolve_trait_property(contexts, property_name),
                "ImplementedTrait" => resolve_implemented_trait_property(contexts, property_name),
                "RawType" | "ResolvedPathType" | "PrimitiveType"
                    if matches!(property_name.as_ref(), "name") =>
                {
                    // fields from "RawType"
                    resolve_raw_type_property(contexts, property_name)
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
                            .expect("token was not a Function")
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

                            match item {
                                Variant::Plain(_) => Box::new(std::iter::empty()),
                                Variant::Tuple(fields) => Box::new(
                                    fields.iter().filter(|x| x.is_some()).map(move |field_id| {
                                        origin.make_item_vertex(
                                            item_index
                                                .get(field_id.as_ref().unwrap())
                                                .expect("missing item"),
                                        )
                                    }),
                                ),
                                Variant::Struct {
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
