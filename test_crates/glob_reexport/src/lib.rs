//! This package exports the following:
//! - `foo`, also as `inner::foo`
//! - `Bar`, also as `inner::Bar`

pub mod inner {
    pub fn foo() {}

    pub struct Bar;
}

pub use inner::*;
