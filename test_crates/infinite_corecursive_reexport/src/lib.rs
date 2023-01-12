//! The below function should be available as:
//! - `a::foo()`
//! - `b::a::foo()`
//! - `a::b::a::foo()`
//! - any sequence that flips between `a` and `b`, and ends with `foo()`

pub mod a {
    pub use crate::b;

    pub fn foo() {}
}

pub mod b {
    pub use crate::a;
}
