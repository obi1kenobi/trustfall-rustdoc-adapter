//! In Rust, type names and value names (including both `fn` and `const`) have different,
//! mutually-disjoint namespaces. It's allowed for those namespaces to have matching names.
//! When the name is used, the surrounding context determines whether it's resolved
//! to the value or to the type by that name.
//!
//! This package exports the following:
//! - the function `Foo`, also as `nested::Foo`
//! - the type `Foo`, also as `nested::Foo`
//! - the const value `Bar`, also as `nested::Bar`
//! - the type `Bar`, also as `nested::Bar`

pub mod nested {
    pub struct Foo {}

    #[allow(non_snake_case)]
    pub fn Foo() {}

    pub struct Bar {}

    #[allow(non_upper_case_globals)]
    pub const Bar: Bar = Bar {};
}

pub use nested::*;
