//! This package exports the following:
//! - `renamed_foo`
//! - `RenamedBar`
//! - `RenamedFirst`
//! - `RenamedOnion`

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

    pub(crate) mod renaming {
        pub use super::deeper::foo as renamed_foo;
        pub use super::deeper::Bar as RenamedBar;
        pub use super::deeper::Baz::First as RenamedFirst;
        pub use super::deeper::Onion as RenamedOnion;
    }
}

pub use nested::renaming::*;
