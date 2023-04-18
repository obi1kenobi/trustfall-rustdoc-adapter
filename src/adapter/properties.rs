use trustfall::{
    provider::{
        accessor_property, field_property, resolve_property_with, ContextIterator,
        ContextOutcomeIterator,
    },
    FieldValue,
};

use super::vertex::Vertex;

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
) -> ContextOutcomeIterator<'a, Vertex<'a>, FieldValue> {
    match property_name {
        "unsafe" => resolve_property_with(contexts, field_property!(as_trait, is_unsafe)),
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
        _ => unreachable!("Trait property {property_name}"),
    }
}
