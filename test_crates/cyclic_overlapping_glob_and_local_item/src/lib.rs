// This crate exports:
// - `Foo` only as itself
// - `inner::Foo` also as `inner::nested::Foo`, `nested::Foo`, and `nested::inner::Foo`.
//
// `inner`'s glob import is a no-op other than to set up an infinite `inner::inner` cycle.
// `nested`'s glob import makes `nested` visible inside itself.
// Combined with the top-level import of `inner::nested::*` it allows
// `nested` to be directly imporable from the root as well.

pub use inner::nested::*;

pub struct Foo {}

pub mod inner {
    pub use super::*;

    pub struct Foo {}

    pub mod nested {
        pub use super::*;
    }
}
