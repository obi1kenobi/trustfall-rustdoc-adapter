// In every scope here, `Foo` and `Bar` switch which structs they refer to.
//
// In the top-level and in the innermost scopes,
// `Foo` refers to `pub struct Foo` and `Bar` refers to `pub struct Bar`.
// But in the middle scope, `Foo` refers to `pub struct Bar`
// and `Bar` refers to `pub struct Foo`.
//
// Each scope publicly exports its `Foo` and `Bar` symbols.
// - `pub struct Foo` is exported as [`Foo`, `inner::Bar`, `inner::nested::Foo`]
// - `pub struct Bar` is exported as [`Bar`, `inner::Foo`, `inner::nested::Bar`]

pub use inner::nested::*;

pub struct Foo;

pub mod inner {
    pub use super::Foo as Bar;
    pub use nested::Bar as Foo;

    pub mod nested {
        pub struct Bar;

        pub use super::super::*;
    }
}
