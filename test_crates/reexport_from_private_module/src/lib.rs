//! This package exports the following:
//! - `foo`
//! - `Bar`
//! - `nested::Baz`
//! - `quux`
//!
//! The `inner` and `inner2` modules are private but the re-exports expose their contents.

mod inner {
    pub fn foo() {}

    pub struct Bar;

    pub mod nested {
        pub struct Baz;
    }
}

pub use inner::*;

mod inner2 {
    pub fn quux() {}
}

pub use inner2::quux;
