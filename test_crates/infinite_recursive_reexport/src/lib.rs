//! The below function should be available as:
//! - `foo()`
//! - `inner::foo()`
//! - `inner::inner::foo()`
//! - any number of consecutive `inner` followed by `foo()`

pub mod inner {
    pub fn foo() {}

    pub use super::inner;
}

pub use crate::inner::foo;
