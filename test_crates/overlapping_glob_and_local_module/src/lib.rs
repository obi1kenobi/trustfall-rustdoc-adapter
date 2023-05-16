// This crate exports:
// - `sibling::duplicated::Foo` only as itself
// - `inner::duplicated::Bar` only as itself.
//
// The glob import is a no-op here since it isn't allowed
// to shadow the locally-defined `duplicated` module.

pub mod sibling {
    pub mod duplicated {
        pub struct Foo;
    }
}

pub mod inner {
    pub use super::sibling::*;

    pub mod duplicated {
        pub struct Bar;
    }
}
