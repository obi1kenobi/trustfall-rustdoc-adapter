//! This package exports the following:
//! - `renamed_foo`
//! - `RenamedBar`
//! - `RenamedFirst`

mod nested {
    mod deeper {
        pub fn foo() {}

        pub struct Bar;

        pub enum Baz {
            First,
        }
    }

    pub(crate) mod renaming {
        pub use super::deeper::foo as renamed_foo;
        pub use super::deeper::Bar as RenamedBar;
        pub use super::deeper::Baz::First as RenamedFirst;
    }
}

pub use nested::renaming::*;
