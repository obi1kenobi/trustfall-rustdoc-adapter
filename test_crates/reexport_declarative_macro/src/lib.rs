#![no_std]

//! This crate exports the following macros:
//! - `top_level_exported`, at top level only
//! - `private_mod_exported`, at top level only
//! - `top_level_reexported`, at top level, and inside
//!   the `macros`, `reexports`, and `glob_reexports` modules
//! - `private_mod_reexported`, at top level, and inside
//!   the `macros`, `reexports`, and `glob_reexports` modules
//!
//! The `top_level_not_exported` and `private_mod_not_exported` macros are not exported.
#![allow(unused_macros)]

macro_rules! top_level_not_exported {
    () => {}
}

#[macro_export]
macro_rules! top_level_exported {
    () => {}
}

#[macro_export]
macro_rules! top_level_reexported {
    () => {}
}

mod private {
    #[macro_export]
    macro_rules! private_mod_exported {
        () => {}
    }

    macro_rules! private_mod_not_exported {
        () => {}
    }

    #[macro_export]
    macro_rules! private_mod_reexported {
        () => {}
    }
}

pub mod macros {
    pub use crate::private_mod_reexported;
    pub use crate::top_level_reexported;
}

pub mod glob_reexports {
    pub use crate::macros::*;
}

pub mod reexports {
    pub use crate::macros::{private_mod_reexported, top_level_reexported};
}
