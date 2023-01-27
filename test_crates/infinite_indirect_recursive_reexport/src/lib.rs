//! The below function should be available as:
//! - `foo()`
//! - `nested::foo()`
//! - `nested::nested::foo()`
//! - any number of consecutive `nested` followed by `foo()`
//!
//! However, we don't actually want to expand the names infinitely.
//! For the purposes of our import tracking, we'll consider only
//! cycle-free import paths, which are the following:
//! - `foo()`
//! - `nested::foo()`

mod nested_other {
    pub use crate::nested;

    pub fn foo() {}
}

pub mod nested {
    pub use crate::nested_other::foo;

    pub use crate::nested_other::nested;
}

pub use nested::foo;
