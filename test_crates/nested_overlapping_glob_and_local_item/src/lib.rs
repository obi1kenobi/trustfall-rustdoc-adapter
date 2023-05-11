// This crate exports:
// - `Foo` only as itself
// - `inner::Foo` also as `inner::nested::Foo`, so `inner`'s glob import is a no-op
//
// Technically, `inner::Foo` is also visible as `inner::inner::Foo` and `inner::nested::inner::Foo`
// and infinitely many other similar names, but we don't return them since they include cycles.

pub struct Foo;

pub mod inner {
    pub use super::*;

    pub struct Foo;

    pub mod nested {
        pub use super::*;
    }
}
