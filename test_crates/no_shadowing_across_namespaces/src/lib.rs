// Glob imports do not import items whose names would conflict
// with locally-defined items with those names.
//
// However, this rule applies per-namespace.
// Types and functions are in different namespaces,
// so there's no overlap there.
//
// Each module here exports *two* items named `Foo`: a struct and a fn.
//
// This crate exports:
// - `pub struct Foo` as [`Foo`, `nested::Foo`]
// - `pub fn Foo()` as [`Foo`, `nested::Foo`]

pub struct Foo;

pub mod nested {
    #[allow(non_snake_case)]
    pub fn Foo() {}

    pub use super::*;
}

pub use nested::*;
