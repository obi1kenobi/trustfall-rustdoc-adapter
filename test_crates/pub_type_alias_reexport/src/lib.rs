//! This package exports the following:
//! - Exported
//!
//! The `inner` module is private so `Foo` is not exported.
//! However, the `Exported` type alias is public and is exported.
//! The `Exported::bar` function is also visible (but not directly exported).
//!
//! As far as anyone outside this crate can tell,
//! the `Foo` type is actually called `Exported` now.

mod inner {
    pub struct Foo;
}

pub type Exported = crate::inner::Foo;
