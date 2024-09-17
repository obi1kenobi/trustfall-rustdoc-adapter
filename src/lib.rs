mod adapter;
mod attributes;
mod indexed_crate;
mod sealed_trait;
mod visibility_tracker;

#[cfg(test)]
pub(crate) mod test_util;

// Re-export the Crate type so we can deserialize it.
pub use rustdoc_types::Crate;

pub use {
    adapter::RustdocAdapter,
    indexed_crate::{ImportablePath, IndexedCrate, PackageHandler, PackageStorage},
};
