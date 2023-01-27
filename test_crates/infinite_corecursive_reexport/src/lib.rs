//! The below function should be available as:
//! - `a::foo()`
//! - `b::a::foo()`
//! - `a::b::a::foo()`
//! - any sequence that flips between `a` and `b`, and ends with `foo()`
//!
//! However, we don't actually want to expand the names infinitely.
//! For the purposes of our import tracking, we'll consider only
//! cycle-free import paths, which are the following:
//! - `a::foo()`
//! - `b::a::foo()`

pub mod a {
    pub use crate::b;

    pub fn foo() {}
}

pub mod b {
    pub use crate::a;
}
