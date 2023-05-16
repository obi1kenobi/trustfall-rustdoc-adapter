/// Example adapted from <https://github.com/rust-lang/rust/issues/111338>
mod first {
    mod a {
        pub(crate) struct Foo(
            // With `pub(super) bool` this does not cause shadowing: the implicit constructor
            // is only `pub(super)` visible itself, which
            // means `second::Foo`'s constructor is public since it isn't shadowed.
            //
            // With `pub(crate) bool` there's shadowing that hides
            // all `crate::Foo` names both in the values and the types namespaces.
            pub(super) bool,
        );
    }

    mod b {
        pub(crate) struct Foo{}
    }

    pub(crate) use a::*;
    pub(crate) use b::*;
}

mod second {
    pub struct Foo();
}

use first::*;  // *** NOT A `pub use` ***
pub use second::*;
