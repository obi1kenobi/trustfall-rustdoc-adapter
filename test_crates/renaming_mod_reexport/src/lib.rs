//! The below function should be available as:
//! - `inner::a::foo()`
//! - `inner::b::foo()`
//! - `direct::foo()`

pub mod inner {
    pub mod a {
        pub fn foo() {}
    }

    pub use self::a as b;
}

pub use crate::inner::b as direct;
