use std::collections::{BTreeSet, HashMap};

use rustdoc_types::{Id, Impl, Item, ItemEnum};
use trustfall::provider::VertexIterator;

use crate::adapter::{Origin, Vertex};

pub(crate) fn resolve_methods_slow_path<'a>(
    impl_vertex: &'a Impl,
    origin: Origin,
    item_index: &'a HashMap<Id, Item>,
) -> VertexIterator<'a, Vertex<'a>> {
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
                        if let Some(name) = next_item.and_then(|x| x.name.as_deref()) {
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

    Box::new(
        provided_methods
            .chain(impl_vertex.items.iter())
            .filter_map(move |item_id| {
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
            }),
    )
}
