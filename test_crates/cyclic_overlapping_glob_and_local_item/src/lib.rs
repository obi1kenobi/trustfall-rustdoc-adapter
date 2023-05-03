// This crate exports:
// - `Foo` only as itself
// - `inner::Foo` also as `inner::nested::Foo`.
//
// `inner`'s glob import is a no-op, and so is the top-level glob import of `inner::nested::*`.

pub use inner::nested::*;

pub struct Foo;

pub mod inner {
    pub use super::*;

    pub struct Foo;

    pub mod nested {
        pub use super::*;
    }
}
