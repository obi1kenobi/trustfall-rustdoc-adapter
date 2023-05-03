// This crate exports:
// - `Foo` only as itself
// - `inner::Foo` also as `inner::nested::Foo`, so `inner`'s glob import is a no-op

pub struct Foo;

pub mod inner {
    pub use super::*;

    pub struct Foo;

    pub mod nested {
        pub use super::*;
    }
}
