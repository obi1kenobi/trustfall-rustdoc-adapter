//! The below function should be available as:
//! - `inner::foo()`
//! - `foo()`
//! - `bar()`

pub mod inner {
    pub fn foo() {}
}

pub use crate::inner::foo;
pub use foo as bar;
