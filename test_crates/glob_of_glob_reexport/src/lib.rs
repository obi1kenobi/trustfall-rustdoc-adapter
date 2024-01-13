//! This package exports the following:
//! - `foo`
//! - `Bar`
//! - `Baz`
//! - `Onion`

mod nested {
    mod deeper {
        pub fn foo() {}

        pub struct Bar;

        pub enum Baz {
            First,
        }

        pub union Onion {
            pub field: usize
        }
    }

    pub(crate) mod sibling {
        pub use super::deeper::*;
    }
}

pub use nested::sibling::*;
