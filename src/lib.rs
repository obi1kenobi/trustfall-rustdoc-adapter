mod adapter;
mod attributes;
mod exported_name;
mod indexed_crate;
mod item_flags;
mod sealed_trait;
mod visibility_tracker;

#[cfg(test)]
pub(crate) mod test_util;

// Re-export the Crate type so we can deserialize it.
pub use rustdoc_types::Crate;

// Re-export `cargo_metadata` since its types are in our public API.
pub use cargo_metadata;

pub use {
    adapter::RustdocAdapter,
    indexed_crate::{ImportablePath, IndexedCrate, PackageIndex, PackageStorage},
};
