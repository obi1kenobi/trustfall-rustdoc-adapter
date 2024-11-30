mod adapter;
mod attributes;
mod indexed_crate;
mod sealed_trait;
mod visibility_tracker;

mod exported_name;
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
