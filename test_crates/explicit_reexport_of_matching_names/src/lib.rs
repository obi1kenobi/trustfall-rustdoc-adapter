//! In Rust, type names and value names (including both `fn` and `const`) have different,
//! mutually-disjoint namespaces. It's allowed for those namespaces to have matching names.
//! When the name is used, the surrounding context determines whether it's resolved
//! to the value or to the type by that name.
//!
//! For the love of all that is good in the world,
//! please *do not* actually write code like this.
//! This is peak "do as I say, not as I do" territory.
//!
//! This package exports the following:
//! - the function `Foo`, also as `Bar` and `nested::Foo`
//! - the type `Foo`, also as `Bar` and `nested::Foo`

pub mod nested {
    pub struct Foo {}

    #[allow(non_snake_case)]
    pub fn Foo() {}
}

pub use nested::Foo;
pub use Foo as Bar;

// Proof that both the type and the function are visible.
// Not exported.
#[allow(dead_code)]
fn proof() -> Bar {
    Bar();
    Bar {}
}
