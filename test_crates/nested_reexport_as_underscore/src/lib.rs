//! This crate does not re-export any *nameable* items.
//!
//! However, glob imports of this file (in the style of a prelude)
//! make `Trait::method()` visible, making `().method()` valid Rust.
//!
//! Docs: <https://doc.rust-lang.org/reference/items/use-declarations.html#underscore-imports>

mod inner {
    pub trait Trait {
        fn method(&self) {}
    }

    impl Trait for () {}
}

mod second {
    pub use super::inner::Trait as _;
}

pub use second::*;

/// Verify that the trait is indeed visible.
#[allow(dead_code)]
fn proof() {
    ().method();
}
