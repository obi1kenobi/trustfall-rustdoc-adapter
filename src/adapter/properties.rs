use rustdoc_types::{ItemEnum, Visibility};
use trustfall::{
    provider::{
        accessor_property, field_property, resolve_property_with, AsVertex, ContextIterator,
        ContextOutcomeIterator,
    },
    FieldValue,
};

use crate::{attributes::Attribute, IndexedCrate};

use super::{origin::Origin, vertex::Vertex};

pub(super) fn resolve_crate_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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

pub(super) fn resolve_item_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "id" => resolve_property_with(
            contexts,
            field_property!(as_item, id, { id.0.clone().into() }),
        ),
        "crate_id" => resolve_property_with(contexts, field_property!(as_item, crate_id)),
        "name" => resolve_property_with(contexts, field_property!(as_item, name)),
        "docs" => resolve_property_with(contexts, field_property!(as_item, docs)),
        "attrs" => resolve_property_with(contexts, field_property!(as_item, attrs)),
        "deprecated" => resolve_property_with(
            contexts,
            field_property!(as_item, deprecation, { deprecation.is_some().into() }),
        ),
        "doc_hidden" => resolve_property_with(
            contexts,
            field_property!(as_item, attrs, {
                attrs
                    .iter()
                    .any(|attr| Attribute::is_doc_hidden(attr))
                    .into()
            }),
        ),
        "public_api_eligible" => resolve_property_with(contexts, move |vertex| {
            // Items are eligible for public API if both:
            // - The item is public, either explicitly (`pub`) or implicitly (like enum variants).
            // - The item is deprecated, or not `#[doc(hidden)]`.
            //
            // This does not mean that the item is necessarily part of the public API!
            // An item that is not eligible by itself cannot be part of the public API,
            // but eligible items might not be public API -- for example, pub-in-priv items
            // (public items in a private module) are eligible but not public API.
            let item = vertex.as_item().expect("vertex was not an Item");
            let is_public = matches!(item.visibility, Visibility::Public | Visibility::Default);
            (is_public
                && (item.deprecation.is_some()
                    || !item.attrs.iter().any(|attr| Attribute::is_doc_hidden(attr))))
            .into()
        }),
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

pub(super) fn resolve_module_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "is_stripped" => resolve_property_with(contexts, field_property!(as_module, is_stripped)),
        _ => unreachable!("Module property {property_name}"),
    }
}

pub(super) fn resolve_struct_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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
                    has_stripped_fields,
                    ..
                } => has_stripped_fields.into(),
                _ => FieldValue::Null,
            }
        }),
        _ => unreachable!("Struct property {property_name}"),
    }
}

pub(super) fn resolve_span_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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

pub(super) fn resolve_enum_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "variants_stripped" => {
            resolve_property_with(contexts, field_property!(as_enum, has_stripped_variants))
        }
        _ => unreachable!("Enum property {property_name}"),
    }
}

pub(super) fn resolve_union_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "fields_stripped" => {
            resolve_property_with(contexts, field_property!(as_union, has_stripped_fields))
        }
        _ => unreachable!("Union property {property_name}"),
    }
}

pub(super) fn resolve_path_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "path" => resolve_property_with(contexts, |vertex| {
            vertex.as_path().expect("not a path").into()
        }),
        _ => unreachable!("Path property {property_name}"),
    }
}

pub(super) fn resolve_importable_path_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "path" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_importable_path()
                .expect("not an importable path")
                .path
                .components
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .into()
        }),
        "visibility_limit" => resolve_property_with(contexts, |_| "public".into()),
        "doc_hidden" => resolve_property_with(
            contexts,
            field_property!(as_importable_path, modifiers, {
                modifiers.doc_hidden.into()
            }),
        ),
        "deprecated" => resolve_property_with(
            contexts,
            field_property!(as_importable_path, modifiers, {
                modifiers.deprecated.into()
            }),
        ),
        "public_api" => {
            resolve_property_with(contexts, accessor_property!(as_importable_path, public_api))
        }
        _ => unreachable!("ImportablePath property {property_name}"),
    }
}

pub(super) fn resolve_function_like_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "const" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.is_const.into() }),
        ),
        "async" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.is_async.into() }),
        ),
        "unsafe" => resolve_property_with(
            contexts,
            field_property!(as_function, header, { header.is_unsafe.into() }),
        ),
        "has_body" => resolve_property_with(
            contexts,
            field_property!(as_function, has_body, { (*has_body).into() }),
        ),
        _ => unreachable!("FunctionLike property {property_name}"),
    }
}

pub(super) fn resolve_function_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "export_name" => resolve_property_with(contexts, move |vertex| {
            let item = vertex.as_item().expect("not an Item vertex");

            if item.attrs.iter().any(|attr| attr == "#[no_mangle]") {
                // Items with `#[no_mangle]` attributes are exported under their item name.
                // Ref: https://doc.rust-lang.org/reference/abi.html#the-no_mangle-attribute
                item.name.clone().into()
            } else {
                // Look for an `#[export_name = "something"]` attribute.
                // Ref: https://doc.rust-lang.org/reference/abi.html#the-export_name-attribute
                item.attrs
                    .iter()
                    .filter_map(|attr| {
                        if attr.starts_with("#[export_name") {
                            let parsed = Attribute::new(attr);
                            if parsed.content.base == "export_name" {
                                parsed
                                    .content
                                    .assigned_item
                                    .map(|name| name.trim_matches('"'))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .next()
                    .into()
            }
        }),
        _ => unreachable!("Function property {property_name}"),
    }
}

pub(super) fn resolve_function_parameter_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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

pub(super) fn resolve_function_abi_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    // Known implemented Rust ABIs:
    // https://github.com/rust-lang/rust/blob/557359f92512ca88b62a602ebda291f17a953002/compiler/rustc_target/src/spec/abi.rs#L74-L110
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            let abi = vertex.as_function_abi().expect("not a FunctionAbi");
            match abi {
                rustdoc_types::Abi::Rust => "Rust",
                rustdoc_types::Abi::C { .. } => "C",
                rustdoc_types::Abi::Cdecl { .. } => "cdecl",
                rustdoc_types::Abi::Stdcall { .. } => "stdcall",
                rustdoc_types::Abi::Fastcall { .. } => "fastcall",
                rustdoc_types::Abi::Aapcs { .. } => "aapcs",
                rustdoc_types::Abi::Win64 { .. } => "win64",
                rustdoc_types::Abi::SysV64 { .. } => "sysv64",
                rustdoc_types::Abi::System { .. } => "system",
                rustdoc_types::Abi::Other(o) => {
                    if let Some(name) = o.strip_suffix("-unwind") {
                        name
                    } else {
                        o.as_str()
                    }
                }
            }
            .into()
        }),
        "raw_name" => resolve_property_with(contexts, |vertex| {
            let abi = vertex.as_function_abi().expect("not a FunctionAbi");
            match abi {
                rustdoc_types::Abi::Rust => "Rust",
                rustdoc_types::Abi::C { unwind: false } => "C",
                rustdoc_types::Abi::C { unwind: true } => "C-unwind",
                rustdoc_types::Abi::Cdecl { unwind: false } => "cdecl",
                rustdoc_types::Abi::Cdecl { unwind: true } => "cdecl-unwind",
                rustdoc_types::Abi::Stdcall { unwind: false } => "stdcall",
                rustdoc_types::Abi::Stdcall { unwind: true } => "stdcall-unwind",
                rustdoc_types::Abi::Fastcall { unwind: false } => "fastcall",
                rustdoc_types::Abi::Fastcall { unwind: true } => "fastcall-unwind",
                rustdoc_types::Abi::Aapcs { unwind: false } => "aapcs",
                rustdoc_types::Abi::Aapcs { unwind: true } => "aapcs-unwind",
                rustdoc_types::Abi::Win64 { unwind: false } => "win64",
                rustdoc_types::Abi::Win64 { unwind: true } => "win64-unwind",
                rustdoc_types::Abi::SysV64 { unwind: false } => "sysv64",
                rustdoc_types::Abi::SysV64 { unwind: true } => "sysv64-unwind",
                rustdoc_types::Abi::System { unwind: false } => "system",
                rustdoc_types::Abi::System { unwind: true } => "system-unwind",
                rustdoc_types::Abi::Other(o) => o.as_str(),
            }
            .into()
        }),
        "unwind" => resolve_property_with(contexts, |vertex| {
            // Per rustc's own implementation that checks for unwind ability:
            // https://github.com/rust-lang/rust/blob/557359f92512ca88b62a602ebda291f17a953002/compiler/rustc_middle/src/ty/layout.rs#L1422-L1488
            let abi = vertex.as_function_abi().expect("not a FunctionAbi");
            match abi {
                rustdoc_types::Abi::Rust => true.into(),
                rustdoc_types::Abi::C { unwind }
                | rustdoc_types::Abi::Cdecl { unwind }
                | rustdoc_types::Abi::Stdcall { unwind }
                | rustdoc_types::Abi::Fastcall { unwind }
                | rustdoc_types::Abi::Aapcs { unwind }
                | rustdoc_types::Abi::Win64 { unwind }
                | rustdoc_types::Abi::SysV64 { unwind }
                | rustdoc_types::Abi::System { unwind } => (*unwind).into(),
                rustdoc_types::Abi::Other(other) => match other.as_str() {
                    abi if abi.ends_with("-unwind") || abi.starts_with("rust-") => true.into(),
                    "ptx-kernel"
                    | "msp430-interrupt"
                    | "x86-interrupt"
                    | "amdgpu-kernel"
                    | "efiapi"
                    | "avr-interrupt"
                    | "avr-non-blocking-interrupt"
                    | "C-cmse-nonsecure-call"
                    | "wasm"
                    | "platform-intrinsic"
                    | "unadjusted" => false.into(),
                    _ => FieldValue::NULL,
                },
            }
        }),
        _ => unreachable!("FunctionAbi property {property_name}"),
    }
}

pub(super) fn resolve_impl_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_impl, is_unsafe)),
        "negative" => resolve_property_with(contexts, field_property!(as_impl, is_negative)),
        "synthetic" => resolve_property_with(contexts, field_property!(as_impl, is_synthetic)),
        _ => unreachable!("Impl property {property_name}"),
    }
}

pub(super) fn resolve_attribute_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "raw_attribute" => {
            resolve_property_with(contexts, accessor_property!(as_attribute, raw_attribute))
        }
        "is_inner" => resolve_property_with(contexts, field_property!(as_attribute, is_inner)),
        _ => unreachable!("Attribute property {property_name}"),
    }
}

pub(super) fn resolve_attribute_meta_item_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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

pub(super) fn resolve_raw_type_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
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

pub(super) fn resolve_trait_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_trait, is_unsafe)),
        "object_safe" => resolve_property_with(contexts, field_property!(as_trait, is_object_safe)),
        "sealed" => resolve_property_with(contexts, move |vertex| {
            let trait_item = vertex.as_item().expect("not an Item");
            let origin = vertex.origin;

            let indexed_crate = match origin {
                Origin::CurrentCrate => current_crate,
                Origin::PreviousCrate => previous_crate.expect("no previous crate provided"),
            };

            indexed_crate.is_trait_sealed(&trait_item.id).into()
        }),
        _ => unreachable!("Trait property {property_name}"),
    }
}

pub(super) fn resolve_implemented_trait_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            let (_, item) = vertex
                .as_implemented_trait()
                .expect("not an ImplementedTrait");

            item.name.clone().into()
        }),
        _ => unreachable!("ImplementedTrait property {property_name}"),
    }
}

pub(crate) fn resolve_static_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "mutable" => resolve_property_with(contexts, field_property!(as_static, is_mutable)),
        _ => unreachable!("Static property {property_name}"),
    }
}

pub(crate) fn resolve_associated_type_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "has_default" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::AssocType { type_, .. } = &inner else {
                    unreachable!("expected to have a AssocType")
                };
                type_.is_some().into()
            }),
        ),
        _ => unreachable!("AssociatedType property {property_name}"),
    }
}

pub(crate) fn resolve_associated_constant_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "default" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::AssocConst { value: default, .. } = &inner else {
                    unreachable!("expected to have a AssocConst")
                };
                default.clone().into()
            }),
        ),
        _ => unreachable!("AssociatedConstant property {property_name}"),
    }
}

pub(crate) fn resolve_constant_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "expr" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::Constant { const_: c, .. } = &inner else {
                    unreachable!("expected to have a Constant")
                };
                c.expr.clone().into()
            }),
        ),
        "value" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::Constant { const_: c, .. } = &inner else {
                    unreachable!("expected to have a Constant")
                };
                c.value.clone().into()
            }),
        ),
        "is_literal" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::Constant { const_: c, .. } = &inner else {
                    unreachable!("expected to have a Constant")
                };
                c.is_literal.into()
            }),
        ),
        _ => unreachable!("Constant property {property_name}"),
    }
}

pub(crate) fn resolve_discriminant_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "value" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_discriminant()
                .expect("vertex was not a Discriminant")
                .to_string()
                .into()
        }),
        _ => unreachable!("Discriminant property {property_name}"),
    }
}

pub(crate) fn resolve_derive_macro_helper_attribute_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_derive_helper_attr()
                .expect("vertex was not a DeriveMacroHelperAttribute")
                .to_string()
                .into()
        }),
        _ => unreachable!("DeriveMacroHelperAttribute property {property_name}"),
    }
}

pub(crate) fn resolve_generic_parameter_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "name" => resolve_property_with(contexts, |vertex| {
            vertex
                .as_generic_parameter()
                .expect("vertex was not a GenericParameter")
                .name
                .clone()
                .into()
        }),
        _ => unreachable!("GenericParameter property {property_name}"),
    }
}

pub(crate) fn resolve_generic_type_parameter_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "has_default" => resolve_property_with(contexts, |vertex| {
            let generic = vertex
                .as_generic_parameter()
                .expect("vertex was not a GenericTypeParameter");

            match &generic.kind {
                rustdoc_types::GenericParamDefKind::Type { default, .. } => {
                    default.is_some().into()
                }
                _ => unreachable!("vertex was not a GenericTypeParameter: {vertex:?}"),
            }
        }),
        "synthetic" => resolve_property_with(contexts, |vertex| {
            let generic = vertex
                .as_generic_parameter()
                .expect("vertex was not a GenericTypeParameter");

            match &generic.kind {
                rustdoc_types::GenericParamDefKind::Type { is_synthetic, .. } => {
                    (*is_synthetic).into()
                }
                _ => unreachable!("vertex was not a GenericTypeParameter: {vertex:?}"),
            }
        }),
        _ => unreachable!("GenericTypeParameter property {property_name}"),
    }
}

pub(crate) fn resolve_generic_const_parameter_property<'a, V: AsVertex<Vertex<'a>> + 'a>(
    contexts: ContextIterator<'a, V>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, V, FieldValue> {
    match property_name {
        "has_default" => resolve_property_with(contexts, |vertex| {
            let generic = vertex
                .as_generic_parameter()
                .expect("vertex was not a GenericConstParameter");

            match &generic.kind {
                rustdoc_types::GenericParamDefKind::Const { default, .. } => {
                    default.is_some().into()
                }
                _ => unreachable!("vertex was not a GenericConstParameter: {vertex:?}"),
            }
        }),
        _ => unreachable!("GenericConstParameter property {property_name}"),
    }
}
