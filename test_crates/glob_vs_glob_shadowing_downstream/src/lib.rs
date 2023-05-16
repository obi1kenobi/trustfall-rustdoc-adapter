// This crate only exports the name `second::Bar`.
// Inside `first`, the name `Foo` conflicts between the two glob exports.
//
// The glob re-export of `first::*` inside `second` brings the conflicting `Foo` names
// into `second`, introducing a conflict with the glob-exported `inner::Foo` as well.
// That leaves `second::Bar` as the only valid re-export.

mod first {
    mod a {
        pub struct Foo;
    }
    mod b {
        pub struct Foo;
    }

    pub use a::*;
    pub use b::*;
}

pub mod second {
    mod inner {
        pub struct Foo;

        pub struct Bar;
    }

    pub use super::first::*;
    pub use inner::*;
}
