//! The below function should be available as both:
//! - `inner::foo()`
//! - `foo()`

pub mod inner {
    pub fn foo() {}
}

pub use crate::inner::foo;
