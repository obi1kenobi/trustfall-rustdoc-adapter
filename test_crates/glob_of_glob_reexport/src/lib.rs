//! This package exports the following:
//! - `foo`
//! - `Bar`
//! - `Baz`

mod nested {
    mod deeper {
        pub fn foo() {}

        pub struct Bar;

        pub enum Baz {
            First,
        }
    }

    pub(crate) mod sibling {
        pub use super::deeper::*;
    }
}

pub use nested::sibling::*;
