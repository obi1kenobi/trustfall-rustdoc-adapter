use std::{borrow::Cow, rc::Rc};

use rustdoc_types::{Abi, Item, Span};

use crate::{
    attributes::{Attribute, AttributeMetaItem},
    indexed_crate::ImportablePath,
};

use super::{
    enum_variant::{EnumVariant, LazyDiscriminants},
    vertex::{Vertex, VertexKind},
};

#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub enum Origin {
    CurrentCrate,
    PreviousCrate,
}

impl Origin {
    pub(super) fn make_item_vertex<'a>(&self, item: &'a Item) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: item.into(),
        }
    }

    pub(super) fn make_span_vertex<'a>(&self, span: &'a Span) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: span.into(),
        }
    }

    pub(super) fn make_path_vertex<'a>(&self, path: &'a [String]) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Path(path),
        }
    }

    pub(super) fn make_importable_path_vertex<'a>(
        &self,
        importable_path: ImportablePath<'a>,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::ImportablePath(Rc::from(importable_path)),
        }
    }

    pub(super) fn make_raw_type_vertex<'a>(&self, raw_type: &'a rustdoc_types::Type) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::RawType(raw_type),
        }
    }

    pub(super) fn make_attribute_vertex<'a>(&self, attr: Attribute<'a>) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Attribute(attr),
        }
    }

    pub(super) fn make_attribute_meta_item_vertex<'a>(
        &self,
        meta_item: Rc<AttributeMetaItem<'a>>,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::AttributeMetaItem(meta_item),
        }
    }

    pub(super) fn make_implemented_trait_vertex<'a>(
        &self,
        path: &'a rustdoc_types::Path,
        trait_def: &'a Item,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::ImplementedTrait(path, trait_def),
        }
    }

    pub(super) fn make_function_parameter_vertex<'a>(&self, name: &'a str) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::FunctionParameter(name),
        }
    }

    pub(super) fn make_function_abi_vertex<'a>(&self, abi: &'a Abi) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: abi.into(),
        }
    }

    pub(super) fn make_discriminant_vertex<'a>(&self, value: Cow<'a, str>) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Discriminant(value),
        }
    }

    pub(super) fn make_variant_vertex<'a>(
        &self,
        item: &'a Item,
        discriminants: Rc<LazyDiscriminants<'a>>,
        index: usize,
    ) -> Vertex<'a> {
        Vertex {
            origin: *self,
            kind: VertexKind::Variant(EnumVariant::new(item, discriminants, index)),
        }
    }
}
