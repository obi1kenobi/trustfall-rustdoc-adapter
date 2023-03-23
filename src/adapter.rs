use std::{collections::BTreeSet, rc::Rc, sync::Arc};

use rustdoc_types::{
    Crate, Enum, Function, Id, Impl, Item, ItemEnum, Method, Path, Span, Struct, Trait, Type,
    Variant,
};
use trustfall::{
    provider::{
        resolve_coercion_with, resolve_neighbors_with, resolve_property_with, Adapter,
        ContextIterator, ContextOutcomeIterator, EdgeParameters, QueryInfo, Typename,
        VertexIterator,
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
                rustdoc_types::ItemEnum::Method(..) => "Method",
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

    fn as_struct_item(&self) -> Option<(&'a Item, &'a Struct)> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Struct(s) => Some((item, s)),
            _ => None,
        })
    }

    fn as_struct_field_item(&self) -> Option<(&'a Item, &'a Type)> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::StructField(s) => Some((item, s)),
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

    fn as_method(&self) -> Option<&'a Method> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Method(func) => Some(func),
            _ => None,
        })
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

fn get_crate_property(crate_vertex: &Vertex, field_name: &str) -> FieldValue {
    let crate_item = crate_vertex.as_crate().expect("vertex was not a Crate");
    match field_name {
        "root" => crate_item.root.0.clone().into(),
        "crate_version" => crate_item.crate_version.clone().into(),
        "includes_private" => crate_item.includes_private.into(),
        "format_version" => crate_item.format_version.into(),
        _ => unreachable!("Crate property {field_name}"),
    }
}

fn get_item_property(item_vertex: &Vertex, field_name: &str) -> FieldValue {
    let item = item_vertex.as_item().expect("vertex was not an Item");
    match field_name {
        "id" => item.id.0.clone().into(),
        "crate_id" => item.crate_id.into(),
        "name" => item.name.clone().into(),
        "docs" => item.docs.clone().into(),
        "attrs" => item.attrs.clone().into(),
        "visibility_limit" => match &item.visibility {
            rustdoc_types::Visibility::Public => "public".into(),
            rustdoc_types::Visibility::Default => "default".into(),
            rustdoc_types::Visibility::Crate => "crate".into(),
            rustdoc_types::Visibility::Restricted { parent: _, path } => {
                format!("restricted ({path})").into()
            }
        },
        _ => unreachable!("Item property {field_name}"),
    }
}

fn get_struct_property(item_vertex: &Vertex, field_name: &str) -> FieldValue {
    let (_, struct_item) = item_vertex
        .as_struct_item()
        .expect("vertex was not a Struct");
    match field_name {
        "struct_type" => match struct_item.kind {
            rustdoc_types::StructKind::Plain { .. } => "plain",
            rustdoc_types::StructKind::Tuple(..) => "tuple",
            rustdoc_types::StructKind::Unit => "unit",
        }
        .into(),
        "fields_stripped" => match struct_item.kind {
            rustdoc_types::StructKind::Plain {
                fields_stripped, ..
            } => fields_stripped.into(),
            _ => FieldValue::Null,
        },
        _ => unreachable!("Struct property {field_name}"),
    }
}

fn get_span_property(item_vertex: &Vertex, field_name: &str) -> FieldValue {
    let span = item_vertex.as_span().expect("vertex was not a Span");
    match field_name {
        "filename" => span
            .filename
            .to_str()
            .expect("non-representable path")
            .into(),
        "begin_line" => (span.begin.0 as u64).into(),
        "begin_column" => (span.begin.1 as u64).into(),
        "end_line" => (span.end.0 as u64).into(),
        "end_column" => (span.end.1 as u64).into(),
        _ => unreachable!("Span property {field_name}"),
    }
}

fn get_enum_property(item_vertex: &Vertex, field_name: &str) -> FieldValue {
    let enum_item = item_vertex.as_enum().expect("vertex was not an Enum");
    match field_name {
        "variants_stripped" => enum_item.variants_stripped.into(),
        _ => unreachable!("Enum property {field_name}"),
    }
}

fn get_path_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let path_vertex = vertex.as_path().expect("vertex was not a Path");
    match field_name {
        "path" => path_vertex.into(),
        _ => unreachable!("Path property {field_name}"),
    }
}

fn get_importable_path_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let path_vertex = vertex
        .as_importable_path()
        .expect("vertex was not an ImportablePath");
    match field_name {
        "path" => path_vertex
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .into(),
        "visibility_limit" => "public".into(),
        _ => unreachable!("ImportablePath property {field_name}"),
    }
}

fn get_function_like_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let maybe_function = vertex.as_function();
    let maybe_method = vertex.as_method();

    let (header, _decl) = maybe_function
        .map(|func| (&func.header, &func.decl))
        .unwrap_or_else(|| {
            let method = maybe_method.unwrap_or_else(|| {
                unreachable!("vertex was neither a function nor a method: {vertex:?}")
            });
            (&method.header, &method.decl)
        });

    match field_name {
        "const" => header.const_.into(),
        "async" => header.async_.into(),
        "unsafe" => header.unsafe_.into(),
        _ => unreachable!("FunctionLike property {field_name}"),
    }
}

fn get_function_parameter_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let function_parameter_vertex = vertex
        .as_function_parameter()
        .expect("vertex was not a FunctionParameter");

    match field_name {
        "name" => function_parameter_vertex.into(),
        _ => unreachable!("FunctionParameter property {field_name}"),
    }
}

fn get_impl_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let impl_vertex = vertex.as_impl().expect("vertex was not an Impl");
    match field_name {
        "unsafe" => impl_vertex.is_unsafe.into(),
        "negative" => impl_vertex.negative.into(),
        "synthetic" => impl_vertex.synthetic.into(),
        _ => unreachable!("Impl property {field_name}"),
    }
}

fn get_attribute_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let attribute = vertex.as_attribute().expect("vertex was not an Attribute");
    match field_name {
        "raw_attribute" => attribute.raw_attribute().into(),
        "is_inner" => attribute.is_inner.into(),
        _ => unreachable!("Attribute property {field_name}"),
    }
}

fn get_attribute_meta_item_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let meta_item = vertex
        .as_attribute_meta_item()
        .expect("vertex was not an AttributeMetaItem");
    match field_name {
        "raw_item" => meta_item.raw_item.into(),
        "base" => meta_item.base.into(),
        "assigned_item" => meta_item.assigned_item.into(),
        _ => unreachable!("Attribute property {field_name}"),
    }
}

fn get_raw_type_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let type_vertex = vertex.as_raw_type().expect("vertex was not a RawType");
    match field_name {
        "name" => match type_vertex {
            rustdoc_types::Type::ResolvedPath(path) => (&path.name).into(),
            rustdoc_types::Type::Primitive(name) => name.into(),
            _ => unreachable!("unexpected RawType vertex content: {type_vertex:?}"),
        },
        _ => unreachable!("RawType property {field_name}"),
    }
}

fn get_trait_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let trait_vertex = vertex.as_trait().expect("vertex was not a Trait");
    match field_name {
        "unsafe" => trait_vertex.is_unsafe.into(),
        _ => unreachable!("Trait property {field_name}"),
    }
}

fn get_implemented_trait_property(vertex: &Vertex, field_name: &str) -> FieldValue {
    let (path, _) = vertex
        .as_implemented_trait()
        .expect("vertex was not a ImplementedTrait");
    match field_name {
        "name" => (&path.name).into(),
        _ => unreachable!("ImplementedTrait property {field_name}"),
    }
}

impl<'a> Adapter<'a> for RustdocAdapter<'a> {
    type Vertex = Vertex<'a>;

    fn resolve_starting_vertices(
        &mut self,
        edge_name: &Arc<str>,
        _parameters: &EdgeParameters,
        _query_info: &QueryInfo,
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
        &mut self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        property_name: &Arc<str>,
        _query_info: &QueryInfo,
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
            let property_name = property_name.clone();
            match type_name.as_ref() {
                "Crate" => resolve_property_with(contexts, move |vertex| {
                    get_crate_property(vertex, property_name.as_ref())
                }),
                "Item" => resolve_property_with(contexts, move |vertex| {
                    get_item_property(vertex, property_name.as_ref())
                }),
                "ImplOwner" | "Struct" | "StructField" | "Enum" | "Variant" | "PlainVariant"
                | "TupleVariant" | "StructVariant" | "Trait" | "Function" | "Method" | "Impl"
                    if matches!(
                        property_name.as_ref(),
                        "id" | "crate_id" | "name" | "docs" | "attrs" | "visibility_limit"
                    ) =>
                {
                    // properties inherited from Item, accesssed on Item subtypes
                    resolve_property_with(contexts, move |vertex| {
                        get_item_property(vertex, property_name.as_ref())
                    })
                }
                "Struct" => resolve_property_with(contexts, move |vertex| {
                    get_struct_property(vertex, property_name.as_ref())
                }),
                "Enum" => resolve_property_with(contexts, move |vertex| {
                    get_enum_property(vertex, property_name.as_ref())
                }),
                "Span" => resolve_property_with(contexts, move |vertex| {
                    get_span_property(vertex, property_name.as_ref())
                }),
                "Path" => resolve_property_with(contexts, move |vertex| {
                    get_path_property(vertex, property_name.as_ref())
                }),
                "ImportablePath" => resolve_property_with(contexts, move |vertex| {
                    get_importable_path_property(vertex, property_name.as_ref())
                }),
                "FunctionLike" | "Function" | "Method"
                    if matches!(property_name.as_ref(), "const" | "unsafe" | "async") =>
                {
                    resolve_property_with(contexts, move |vertex| {
                        get_function_like_property(vertex, property_name.as_ref())
                    })
                }
                "FunctionParameter" => resolve_property_with(contexts, move |vertex| {
                    get_function_parameter_property(vertex, property_name.as_ref())
                }),
                "Impl" => resolve_property_with(contexts, move |vertex| {
                    get_impl_property(vertex, property_name.as_ref())
                }),
                "Attribute" => resolve_property_with(contexts, move |vertex| {
                    get_attribute_property(vertex, property_name.as_ref())
                }),
                "AttributeMetaItem" => resolve_property_with(contexts, move |vertex| {
                    get_attribute_meta_item_property(vertex, property_name.as_ref())
                }),
                "Trait" => resolve_property_with(contexts, move |vertex| {
                    get_trait_property(vertex, property_name.as_ref())
                }),
                "ImplementedTrait" => resolve_property_with(contexts, move |vertex| {
                    get_implemented_trait_property(vertex, property_name.as_ref())
                }),
                "RawType" | "ResolvedPathType" | "PrimitiveType"
                    if matches!(property_name.as_ref(), "name") =>
                {
                    // fields from "RawType"
                    resolve_property_with(contexts, move |vertex| {
                        get_raw_type_property(vertex, property_name.as_ref())
                    })
                }
                _ => unreachable!("resolve_property {type_name} {property_name}"),
            }
        }
    }

    fn resolve_neighbors(
        &mut self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        _query_info: &QueryInfo,
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
                                        | rustdoc_types::ItemEnum::Method(..)
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
                        .as_struct_item()
                        .map(|(_, s)| &s.impls)
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
                resolve_neighbors_with(contexts, |vertex| {
                    let origin = vertex.origin;
                    let decl = vertex.as_function().map(|f| &f.decl).unwrap_or_else(|| {
                        &vertex
                            .as_method()
                            .expect("vertex was neither a Function nor a Method")
                            .decl
                    });

                    Box::new(
                        decl.inputs
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
                        let (_, struct_item) =
                            vertex.as_struct_item().expect("vertex was not a Struct");

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
                    let (_, field_type) = vertex
                        .as_struct_field_item()
                        .expect("not a StructField vertex");
                    Box::new(std::iter::once(origin.make_raw_type_vertex(field_type)))
                }),
                _ => {
                    unreachable!("resolve_neighbors {type_name} {edge_name} {parameters:?}")
                }
            },
            "Impl" => match edge_name.as_ref() {
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

                                let trait_path = impl_vertex
                                    .trait_
                                    .as_ref()
                                    .expect("no trait but provided_trait_methods was non-empty");
                                let trait_item = item_index.get(&trait_path.id);

                                if let Some(trait_item) = trait_item {
                                    if let ItemEnum::Trait(trait_item) = &trait_item.inner {
                                        Box::new(trait_item.items.iter().filter(move |item_id| {
                                            let next_item = &item_index.get(item_id);
                                            if let Some(name) =
                                                next_item.and_then(|x| x.name.as_deref())
                                            {
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
                                        rustdoc_types::ItemEnum::Method(..) => {
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
                            if let Some(item) = item_index.get(&path.id) {
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
        &mut self,
        contexts: ContextIterator<'a, Self::Vertex>,
        type_name: &Arc<str>,
        coerce_to_type: &Arc<str>,
        _query_info: &QueryInfo,
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
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;

    use rustdoc_types::Crate;
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct RustdocFormatVersion {
        format_version: u32,
    }

    fn load_rustdoc_from_file(path: &Path) -> Crate {
        // Parsing JSON after fully reading a file into memory is much faster than
        // parsing directly from a file, even if buffered:
        // https://github.com/serde-rs/json/issues/160
        let mut s = String::new();
        File::open(path)
            .expect("couldn't open file")
            .read_to_string(&mut s)
            .expect("couldn't read file");

        match serde_json::from_str(&s) {
            Ok(c) => c,
            Err(e) => {
                let version = serde_json::from_str::<RustdocFormatVersion>(&s)
                    .expect("unrecognized rustdoc format");

                assert_ne!(rustdoc_types::FORMAT_VERSION, version.format_version, "The format versions matched but the file failed to parse. Perhaps it is corrupted?");
                panic!(
                    "Expected to find rustdoc v{} but got v{} instead.\n\nUnderlying error:\n{e}",
                    rustdoc_types::FORMAT_VERSION,
                    version.format_version
                );
            }
        }
    }

    #[test]
    fn rustdoc_json_format_version() {
        let _version = rustdoc_types::FORMAT_VERSION;
        let current_crate = load_rustdoc_from_file(Path::new(
            &"./localdata/test_data/reexport/rustdoc.json".to_string(),
        ));

        assert_eq!(current_crate.format_version, rustdoc_types::FORMAT_VERSION);
    }
}
