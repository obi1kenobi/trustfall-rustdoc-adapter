mod adapter;
mod attributes;
mod indexed_crate;

#[cfg(test)]
pub(crate) mod test_util;

mod visibility_tracker;

// Re-export the Crate type so we can deserialize it.
pub use rustdoc_types::Crate;

pub use {
    adapter::RustdocAdapter,
    indexed_crate::{ImportablePath, IndexedCrate},
};
