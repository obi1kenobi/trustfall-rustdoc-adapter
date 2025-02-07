use rustdoc_types::{Item, Type};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct StructField<'a> {
    item: &'a Item,
    index: usize,
}

impl<'a> StructField<'a> {
    pub(super) fn new(item: &'a Item, index: usize) -> Self {
        Self { item, index }
    }

    #[inline]
    pub(super) fn item(&self) -> &'a Item {
        self.item
    }

    pub(super) fn raw_type(&self) -> &'a Type {
        match &self.item.inner {
            rustdoc_types::ItemEnum::StructField(field) => field,
            _ => unreachable!("Item was not a StructField"),
        }
    }

    pub(super) fn position(&self) -> i64 {
        self.index as i64 + 1
    }
}
