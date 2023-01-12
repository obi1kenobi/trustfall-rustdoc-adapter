//! This package exports the following:
//! - `First`
//! - `Second`

mod nested {
    pub enum Foo {
        First,
        Second,
    }
}

pub use crate::nested::Foo::*;
