//! This crate re-exports only the struct `Example`.
//!
//! Even though `second` imports two items named `Example`, one of them is renamed to `_`
//! making it unnameable.
//!
//! Docs: <https://doc.rust-lang.org/reference/items/use-declarations.html#underscore-imports>

mod inner {
    pub trait Example {
        fn method(&self) {}
    }
}

mod inner2 {
    pub struct Example {}
}

mod second {
    pub use super::inner::Example as _;
    pub use super::inner2::Example;
}

pub use second::*;
