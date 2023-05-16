// This crate exports:
// - `sibling::duplicated::Foo` only as itself
// - `inner::duplicated::Bar` only as itself.
//
// The glob import is a no-op here since it isn't allowed
// to shadow the explicitly reexported and renamed `duplicated` module.

pub mod sibling {
    pub mod duplicated {
        pub struct Foo;
    }
}

pub mod inner {
    // The re-export of duplicated through here is ignored,
    // since it would conflict with the re-export below.
    pub use super::sibling::*;

    // This re-export of duplicated takes precedence.
    pub use super::outer::contents as duplicated;
}

mod outer {
    pub mod contents {
        pub struct Bar;
    }
}
