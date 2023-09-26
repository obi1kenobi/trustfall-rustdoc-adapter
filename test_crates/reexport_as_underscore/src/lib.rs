//! This crate re-exports only the name `Struct`.
//!
//! However, glob imports of this file (in the style of a prelude)
//! also make `Trait::method()` visible as well, making `().method()` valid Rust.
//!
//! The `_` re-export of the module `hidden` is not nameable at all, and here has no effect.
//! `hidden::UnderscoreImported` is not nameable outside this crate.
//!
//! Docs: <https://doc.rust-lang.org/reference/items/use-declarations.html#underscore-imports>

mod inner {
    pub trait Trait {
        fn method(&self) {}
    }

    impl Trait for () {}

    pub struct Struct {}
}

mod nested {
    pub mod hidden {
        pub struct UnderscoreImported;
    }
}

pub use inner::{
    Struct,
    Trait as _,
};

pub use nested::hidden as _;
