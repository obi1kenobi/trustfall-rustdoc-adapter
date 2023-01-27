//! This package exports the following:
//! - `first::foo`, also as `second::foo`
//! - `second::Bar`, also as `first::Bar`

pub mod first {
    pub fn foo() {}

    pub use crate::second::*;
}

pub mod second {
    pub struct Bar;

    pub use crate::first::*;
}
