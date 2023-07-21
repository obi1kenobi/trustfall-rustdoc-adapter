use rustdoc_types::{ItemEnum, GenericBound};
use trustfall::{
    provider::{
        accessor_property, field_property, resolve_property_with, ContextIterator,
        ContextOutcomeIterator,
    },
    FieldValue,
};

use crate::IndexedCrate;

use super::{vertex::Vertex, origin::Origin};

pub(super) fn resolve_crate_property<'a>(
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

pub(super) fn resolve_item_property<'a>(
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

pub(super) fn resolve_struct_property<'a>(
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

pub(super) fn resolve_span_property<'a>(
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

pub(super) fn resolve_enum_property<'a>(
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

pub(super) fn resolve_path_property<'a>(
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

pub(super) fn resolve_importable_path_property<'a>(
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

pub(super) fn resolve_function_like_property<'a>(
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

pub(super) fn resolve_function_parameter_property<'a>(
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

pub(super) fn resolve_impl_property<'a>(
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

pub(super) fn resolve_attribute_property<'a>(
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

pub(super) fn resolve_attribute_meta_item_property<'a>(
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

pub(super) fn resolve_raw_type_property<'a>(
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

pub(super) fn resolve_trait_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
    current_crate: &'a IndexedCrate<'a>,
    previous_crate: Option<&'a IndexedCrate<'a>>,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_trait, is_unsafe)),
        "sealed" => resolve_property_with(contexts, |vertex| {
            let origin_crate = match vertex.origin {
                Origin::CurrentCrate => current_crate,
                Origin::PreviousCrate => previous_crate.expect("no baseline provided"),
            };

            let trait_item = vertex.as_trait().expect("not a Trait vertex");

            // Background on trait sealing:
            // https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/
            // https://old.reddit.com/r/rust/comments/12cj6as/a_definitive_guide_to_sealed_traits_in_rust/jf21zsm/
            //
            // A trait is sealed if any of the following cases are true:
            // 1. The trait has a pub-in-priv supertrait (i.e. non-importable)
            //    since external impls cannot impl the supertrait.
            let non_importable_supertrait = trait_item.bounds.iter().any(|bound| {
                match bound {
                    GenericBound::TraitBound { trait_, generic_params, modifier } => {
                        let supertrait_id = &trait_.id;

                        // If the supertrait is part of this crate,
                        // but has no importable names, then it must be pub-in-priv.
                        // That means `trait_item` is sealed.
                        if origin_crate.inner.index.contains_key(supertrait_id) && origin_crate.publicly_importable_names(supertrait_id).is_empty() {
                            return true;
                        }

                        false
                    }
                    GenericBound::Outlives(_) => false,
                }
            });

            // 2. The trait has a method without a default implementation that satisfies any of:
            //    - takes an argument whose type is pub-in-priv
            //    - returns a value whose type is pub-in-priv
            //    - has a trait bound where the trait is pub-in-priv
            let sealed_method = trait_item.items
                .iter()
                .filter_map(|id| origin_crate.inner.index.get(id))
                .any(|item| {
                    if let rustdoc_types::Function { decl, generics, has_body, .. } = &item.inner {
                        if has_body {
                            // This method has a default implementation, so it is not sealed.
                            return false;
                        }

                        let sealed_due_to_arg = decl.inputs.iter().any(|(_, ty)| {
                            match ty {
                                rustdoc_types::Type::ResolvedPath(_) => todo!(),
                                rustdoc_types::Type::DynTrait(_) => todo!(),
                                rustdoc_types::Type::Generic(_) => todo!(),
                                rustdoc_types::Type::Primitive(_) => todo!(),
                                rustdoc_types::Type::FunctionPointer(_) => todo!(),
                                rustdoc_types::Type::Tuple(_) => todo!(),
                                rustdoc_types::Type::Slice(_) => todo!(),
                                rustdoc_types::Type::Array { type_, len } => todo!(),
                                rustdoc_types::Type::ImplTrait(_) => todo!(),
                                rustdoc_types::Type::Infer => todo!(),
                                rustdoc_types::Type::RawPointer { mutable, type_ } => todo!(),
                                rustdoc_types::Type::BorrowedRef { lifetime, mutable, type_ } => todo!(),
                                rustdoc_types::Type::QualifiedPath { name, args, self_type, trait_ } => todo!(),
                            }
                        })
                    } else {
                        false
                    }
                })
                .any(|item| {

                })

            non_importable_supertrait.into()
        }),
        _ => unreachable!("Trait property {property_name}"),
    }
}

pub(super) fn resolve_implemented_trait_property<'a>(
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
        _ => unreachable!("ImplementedTrait property {property_name}"),
    }
}

pub(crate) fn resolve_static_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "mutable" => resolve_property_with(contexts, field_property!(as_static, mutable)),
        _ => unreachable!("Static property {property_name}"),
    }
}

pub(crate) fn resolve_associated_type_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "has_default" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::AssocType { default, .. } = &inner else {
                    unreachable!("expected to have a AssocType")
                };
                default.is_some().into()
            }),
        ),
        _ => unreachable!("AssociatedType property {property_name}"),
    }
}

pub(crate) fn resolve_associated_constant_property<'a>(
    contexts: ContextIterator<'a, Vertex<'a>>,
    property_name: &str,
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "default" => resolve_property_with(
            contexts,
            field_property!(as_item, inner, {
                let ItemEnum::AssocConst { default, .. } = &inner else {
                    unreachable!("expected to have a AssocConst")
                };
                default.clone().into()
            }),
        ),
        _ => unreachable!("AssociatedConstant property {property_name}"),
    }
}
