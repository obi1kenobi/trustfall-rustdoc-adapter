//! The below function should be available as:
//! - `foo()`
//! - `inner::foo()`
//! - `inner::inner::foo()`
//! - any number of consecutive `inner` followed by `foo()`
//!
//! However, we don't actually want to expand the names infinitely.
//! For the purposes of our import tracking, we'll consider only
//! cycle-free import paths, which are the following:
//! - `foo()`
//! - `inner::foo()`

pub mod inner {
    pub fn foo() {}

    pub use super::inner;
}

pub use crate::inner::foo;
