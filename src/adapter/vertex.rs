use std::{borrow::Cow, rc::Rc};

use rustdoc_types::{
    Abi, Constant, Crate, Enum, Function, Impl, Item, Module, Path, Span, Static, Struct, Trait,
    Type, Union, VariantKind,
};
use trustfall::provider::Typename;

use crate::{
    attributes::{Attribute, AttributeMetaItem},
    indexed_crate::ImportablePath,
    IndexedCrate,
};

use super::{enum_variant::EnumVariant, origin::Origin};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Vertex<'a> {
    pub(super) origin: Origin,
    pub(super) kind: VertexKind<'a>,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum VertexKind<'a> {
    CrateDiff((&'a IndexedCrate<'a>, &'a IndexedCrate<'a>)),
    Crate(&'a IndexedCrate<'a>),
    Item(&'a Item),
    Span(&'a Span),
    Path(&'a [String]),
    ImportablePath(Rc<ImportablePath<'a>>),
    RawType(&'a Type),
    Attribute(Attribute<'a>),
    AttributeMetaItem(Rc<AttributeMetaItem<'a>>),
    ImplementedTrait(&'a Path, &'a Item),
    FunctionParameter(&'a str),
    FunctionAbi(&'a Abi),
    Discriminant(Cow<'a, str>),
    Variant(EnumVariant<'a>),
}

impl<'a> Typename for Vertex<'a> {
    /// The name of the actual runtime type of this vertex,
    /// intended to fulfill resolution requests for the __typename property.
    #[inline]
    fn typename(&self) -> &'static str {
        match &self.kind {
            VertexKind::Item(item) => match &item.inner {
                rustdoc_types::ItemEnum::Module { .. } => "Module",
                rustdoc_types::ItemEnum::Struct(..) => "Struct",
                rustdoc_types::ItemEnum::Enum(..) => "Enum",
                rustdoc_types::ItemEnum::Union(..) => "Union",
                rustdoc_types::ItemEnum::Function(..) => "Function",
                rustdoc_types::ItemEnum::Variant(variant) => match variant.kind {
                    VariantKind::Plain => "PlainVariant",
                    VariantKind::Tuple(..) => "TupleVariant",
                    VariantKind::Struct { .. } => "StructVariant",
                },
                rustdoc_types::ItemEnum::StructField(..) => "StructField",
                rustdoc_types::ItemEnum::Impl(..) => "Impl",
                rustdoc_types::ItemEnum::Trait(..) => "Trait",
                rustdoc_types::ItemEnum::Constant { .. } => "Constant",
                rustdoc_types::ItemEnum::Static(..) => "Static",
                rustdoc_types::ItemEnum::AssocType { .. } => "AssociatedType",
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
                _ => "RawType",
            },
            VertexKind::FunctionParameter(..) => "FunctionParameter",
            VertexKind::FunctionAbi(..) => "FunctionAbi",
            VertexKind::Discriminant(..) => "Discriminant",
            VertexKind::Variant(ev) => match ev.variant().kind {
                VariantKind::Plain => "PlainVariant",
                VariantKind::Tuple(..) => "TupleVariant",
                VariantKind::Struct { .. } => "StructVariant",
            },
        }
    }
}

#[allow(dead_code)]
impl<'a> Vertex<'a> {
    pub(super) fn new_crate(origin: Origin, crate_: &'a IndexedCrate<'a>) -> Self {
        Self {
            origin,
            kind: VertexKind::Crate(crate_),
        }
    }

    pub(super) fn as_crate_diff(&self) -> Option<(&'a IndexedCrate<'a>, &'a IndexedCrate<'a>)> {
        match &self.kind {
            VertexKind::CrateDiff(tuple) => Some(*tuple),
            _ => None,
        }
    }

    pub(super) fn as_indexed_crate(&self) -> Option<&'a IndexedCrate<'a>> {
        match self.kind {
            VertexKind::Crate(c) => Some(c),
            _ => None,
        }
    }

    pub(super) fn as_crate(&self) -> Option<&'a Crate> {
        self.as_indexed_crate().map(|c| c.inner)
    }

    pub(super) fn as_item(&self) -> Option<&'a Item> {
        match &self.kind {
            VertexKind::Item(item) => Some(item),
            VertexKind::Variant(variant) => Some(variant.item()),
            _ => None,
        }
    }

    pub(super) fn as_module(&self) -> Option<&'a Module> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Module(m) => Some(m),
            _ => None,
        })
    }

    pub(super) fn as_struct(&self) -> Option<&'a Struct> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Struct(s) => Some(s),
            _ => None,
        })
    }

    pub(super) fn as_struct_field(&self) -> Option<&'a Type> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::StructField(s) => Some(s),
            _ => None,
        })
    }

    pub(super) fn as_span(&self) -> Option<&'a Span> {
        match self.kind {
            VertexKind::Span(s) => Some(s),
            _ => None,
        }
    }

    pub(super) fn as_enum(&self) -> Option<&'a Enum> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Enum(e) => Some(e),
            _ => None,
        })
    }

    pub(super) fn as_union(&self) -> Option<&'a Union> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Union(u) => Some(u),
            _ => None,
        })
    }

    pub(super) fn as_trait(&self) -> Option<&'a Trait> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Trait(t) => Some(t),
            _ => None,
        })
    }

    pub(super) fn as_variant(&self) -> Option<&'_ EnumVariant<'a>> {
        match &self.kind {
            VertexKind::Variant(variant) => Some(variant),
            _ => None,
        }
    }

    pub(super) fn as_path(&self) -> Option<&'a [String]> {
        match &self.kind {
            VertexKind::Path(path) => Some(*path),
            _ => None,
        }
    }

    pub(super) fn as_importable_path(&self) -> Option<&'_ ImportablePath<'a>> {
        match &self.kind {
            VertexKind::ImportablePath(path) => Some(path),
            _ => None,
        }
    }

    pub(super) fn as_function(&self) -> Option<&'a Function> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Function(func) => Some(func),
            _ => None,
        })
    }

    pub(super) fn as_function_parameter(&self) -> Option<&'a str> {
        match &self.kind {
            VertexKind::FunctionParameter(name) => Some(name),
            _ => None,
        }
    }

    pub(super) fn as_function_abi(&self) -> Option<&'a Abi> {
        match self.kind {
            VertexKind::FunctionAbi(abi) => Some(abi),
            _ => None,
        }
    }

    pub(super) fn as_impl(&self) -> Option<&'a Impl> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Impl(x) => Some(x),
            _ => None,
        })
    }

    pub(super) fn as_constant(&self) -> Option<&'a Constant> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Constant { const_, .. } => Some(const_),
            _ => None,
        })
    }

    pub(super) fn as_static(&self) -> Option<&'a Static> {
        self.as_item().and_then(|item| match &item.inner {
            rustdoc_types::ItemEnum::Static(c) => Some(c),
            _ => None,
        })
    }

    pub(super) fn as_attribute(&self) -> Option<&'_ Attribute<'a>> {
        match &self.kind {
            VertexKind::Attribute(attr) => Some(attr),
            _ => None,
        }
    }

    pub(super) fn as_attribute_meta_item(&self) -> Option<&'_ AttributeMetaItem<'a>> {
        match &self.kind {
            VertexKind::AttributeMetaItem(meta_item) => Some(meta_item),
            _ => None,
        }
    }

    pub(super) fn as_raw_type(&self) -> Option<&'a rustdoc_types::Type> {
        match &self.kind {
            VertexKind::RawType(ty) => Some(*ty),
            _ => None,
        }
    }

    pub(super) fn as_implemented_trait(&self) -> Option<(&'a rustdoc_types::Path, &'a Item)> {
        match &self.kind {
            VertexKind::ImplementedTrait(path, trait_item) => Some((*path, *trait_item)),
            _ => None,
        }
    }

    pub(super) fn as_discriminant(&self) -> Option<&Cow<'a, str>> {
        match &self.kind {
            VertexKind::Discriminant(variant) => Some(variant),
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

impl<'a> From<&'a Abi> for VertexKind<'a> {
    fn from(a: &'a Abi) -> Self {
        Self::FunctionAbi(a)
    }
}
