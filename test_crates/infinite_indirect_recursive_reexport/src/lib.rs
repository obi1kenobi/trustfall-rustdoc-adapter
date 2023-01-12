//! The below function should be available as:
//! - `foo()`
//! - `inner::foo()`
//! - `inner::inner::foo()`
//! - any number of consecutive `inner` followed by `foo()`

mod nested_other {
    pub use crate::nested;

    pub fn foo() {}
}

pub mod nested {
    pub use crate::nested_other::foo;

    pub use crate::nested_other::nested;
}

pub use nested::foo;
