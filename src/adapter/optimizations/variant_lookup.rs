use std::rc::Rc;

use rustdoc_types::{Id, Item, ItemEnum, VariantKind};
use trustfall::FieldValue;
use trustfall::provider::{
    AsVertex, CandidateValue, ContextIterator, ContextOutcomeIterator, ResolveEdgeInfo, VertexInfo,
    VertexIterator, resolve_neighbors_with,
};

use crate::hashtables::HashMap;

use super::super::{
    RustdocAdapter, enum_variant::LazyDiscriminants, origin::Origin, vertex::Vertex,
};

fn compute_variant_discriminants<'a>(
    outer_item: &'a Item,
    item_index: &'a HashMap<Id, Item>,
) -> Option<Rc<LazyDiscriminants<'a>>> {
    // Discriminants are only well-defined if either:
    // - the enum has a defined `repr` binary representation, or
    // - none of the enum variants contain any fields of their own.
    let has_repr = outer_item.attrs.iter().any(|attr| match attr {
        rustdoc_types::Attribute::Repr(attr_repr) => attr_repr.int.is_some(),
        _ => false,
    });

    let enum_item = match &outer_item.inner {
        ItemEnum::Enum(x) => x,
        _ => unreachable!("Item {outer_item:?} is not an Enum"),
    };

    let mut has_fields_in_variants = false;
    let variants = enum_item
        .variants
        .iter()
        .map(|field_id| {
            let inner = &item_index.get(field_id).expect("missing item").inner;
            match inner {
                ItemEnum::Variant(v) => {
                    match &v.kind {
                        VariantKind::Plain => {}
                        VariantKind::Tuple(t) => has_fields_in_variants |= !t.is_empty(),
                        VariantKind::Struct { fields, .. } => {
                            has_fields_in_variants |= fields.is_empty()
                        }
                    }
                    v
                }
                _ => unreachable!("Item {inner:?} not a Variant"),
            }
        })
        .collect();

    if has_repr || !has_fields_in_variants {
        Some(Rc::new(LazyDiscriminants::new(variants)))
    } else {
        None
    }
}

fn resolve_enum_variants_slow_path<'a>(
    origin: Origin,
    item_vertex: &'a Item,
    item_index: &'a HashMap<Id, Item>,
) -> VertexIterator<'a, Vertex<'a>> {
    let discriminants = compute_variant_discriminants(item_vertex, item_index);
    let enum_item = match &item_vertex.inner {
        ItemEnum::Enum(x) => x,
        _ => unreachable!("Item {item_vertex:?} is not an Enum"),
    };

    Box::new(
        enum_item
            .variants
            .iter()
            .enumerate()
            .map(move |(index, field_id)| {
                origin.make_variant_vertex(
                    item_index.get(field_id).expect("missing item"),
                    discriminants.clone(),
                    index,
                )
            }),
    )
}

fn resolve_enum_variant_by_name<'a>(
    origin: Origin,
    item_vertex: &'a Item,
    name: FieldValue,
    variant_name_index: &'a HashMap<(Id, &str), (&'a Item, usize)>,
    item_index: &'a HashMap<Id, Item>,
) -> VertexIterator<'a, Vertex<'a>> {
    match name {
        FieldValue::String(name) => match variant_name_index.get(&(item_vertex.id, &name)) {
            Some((item, index)) => {
                let discriminants = compute_variant_discriminants(item_vertex, item_index);
                Box::new(std::iter::once(origin.make_variant_vertex(
                    item,
                    discriminants.clone(),
                    *index,
                )))
            }
            None => Box::new(std::iter::empty()),
        },
        _ => Box::new(std::iter::empty()),
    }
}

fn resolve_enum_variant_by_candidate_value<'a>(
    origin: Origin,
    item_vertex: &'a Item,
    candidate: CandidateValue<FieldValue>,
    variant_name_index: &'a HashMap<(Id, &str), (&'a Item, usize)>,
    item_index: &'a HashMap<Id, Item>,
) -> VertexIterator<'a, Vertex<'a>> {
    match candidate {
        CandidateValue::Impossible => Box::new(std::iter::empty()),
        CandidateValue::Single(name) => {
            resolve_enum_variant_by_name(origin, item_vertex, name, variant_name_index, item_index)
        }
        CandidateValue::Multiple(values) => Box::new(values.into_iter().flat_map(move |name| {
            resolve_enum_variant_by_name(origin, item_vertex, name, variant_name_index, item_index)
        })),
        _ => resolve_enum_variants_slow_path(origin, item_vertex, item_index),
    }
}

pub(crate) fn resolve_enum_variant<'a, V: AsVertex<Vertex<'a>> + 'a>(
    adapter: &'a RustdocAdapter<'a>,
    contexts: ContextIterator<'a, V>,
    resolve_info: &ResolveEdgeInfo,
) -> ContextOutcomeIterator<'a, V, VertexIterator<'a, Vertex<'a>>> {
    // If the name of the variant is required to be a particular value, we can
    // use the variant_name_index to find the corresponding variant.
    //
    // There's no advantage in our implementation between knowing values
    // statically vs dynamically, so we check the dynamic case first since
    // it might be more specific.
    if let Some(dynamic_value) = resolve_info
        .destination()
        .dynamically_required_property("name")
    {
        dynamic_value.resolve_with(&adapter, contexts, move |vertex, candidate| {
            let origin = vertex.origin;
            let item_vertex = vertex.as_item().expect("vertex is not an item.");
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let variant_name_index = adapter
                .crate_at_origin(origin)
                .own_crate
                .variant_name_index
                .as_ref()
                .expect("variant_name_index was never constructed");

            resolve_enum_variant_by_candidate_value(
                origin,
                item_vertex,
                candidate,
                variant_name_index,
                item_index,
            )
        })
    } else if let Some(name_value) = resolve_info
        .destination()
        .statically_required_property("name")
    {
        resolve_neighbors_with(contexts, move |vertex| {
            let origin = vertex.origin;
            let item_vertex = vertex.as_item().expect("vertex is not an item.");
            let item_index = &adapter.crate_at_origin(origin).own_crate.inner.index;

            let variant_name_index = adapter
                .crate_at_origin(origin)
                .own_crate
                .variant_name_index
                .as_ref()
                .expect("variant_name_index was never constructed");

            resolve_enum_variant_by_candidate_value(
                origin,
                item_vertex,
                name_value.clone(),
                variant_name_index,
                item_index,
            )
        })
    } else {
        resolve_neighbors_with(contexts, move |vertex| {
            let item_index = &adapter.crate_at_origin(vertex.origin).own_crate.inner.index;
            let item_vertex = vertex.as_item().expect("vertex is not an item.");
            resolve_enum_variants_slow_path(vertex.origin, item_vertex, item_index)
        })
    }
}
