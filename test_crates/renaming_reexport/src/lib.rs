//! The below function should be available as both:
//! - `inner::foo()`
//! - `bar()`

pub mod inner {
    pub fn foo() {}
}

pub use crate::inner::foo as bar;
