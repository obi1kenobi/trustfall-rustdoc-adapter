mod adapter;
mod indexed_crate;

// Re-export the Crate type so we can deserialize it.
pub use rustdoc_types::Crate;

pub use {adapter::RustdocAdapter, indexed_crate::IndexedCrate};
