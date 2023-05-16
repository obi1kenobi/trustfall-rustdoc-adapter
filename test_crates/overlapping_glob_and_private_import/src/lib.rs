// This crate does not export any names!
// The only name it could export is `sibling::Foo`, but shadowing prevents it.

mod sibling {
    pub struct Foo;
}

pub mod inner {
    mod nested {
        pub(super) struct Foo;
    }

    // The re-export of `Foo` through here is ignored,
    // since it would conflict with the import below.
    pub use super::sibling::*;

    // This import of `Foo` takes precedence.
    #[allow(dead_code)]
    use nested::Foo;
}
