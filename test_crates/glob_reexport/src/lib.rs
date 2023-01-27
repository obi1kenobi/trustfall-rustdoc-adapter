//! This package exports the following:
//! - `foo`, also as `inner::foo`
//! - `Bar`, also as `inner::Bar`
//! - `nested`
//! - `Baz` (due to the glob import)
//! - `First`
//! - `Second`

pub mod inner {
    pub fn foo() {}

    pub struct Bar;
}

pub use inner::*;

pub mod nested {
    pub(crate) mod deeper {
        pub enum Baz {
            First,
            Second,
        }
    }
}

pub use nested::deeper::Baz::*;
pub use nested::deeper::*;
