#![no_std]
#![allow(unused_imports)]

//! This crate exercises enum variant import paths across namespaces, reexports,
//! doc-hidden/deprecated modifiers, and glob shadowing.
//! Expected behavior: variants remain importable where allowed, inherit reexport modifiers,
//! and obey namespace-specific shadowing rules.
//!
//! Exported names (paths are relative to the crate root):
//! - `Base` and its variants via `Base::{Plain, Deprecated, Hidden, DeprecatedHidden}`.
//! - `Plain`, `Deprecated`, `Hidden`, `DeprecatedHidden` via `pub use Base::*` (glob reexport).
//! - `RenamedPlain`, `HiddenPlain`, `DeprecatedPlain` via explicit reexports of `Base::Plain`,
//!   carrying the corresponding rename and doc/deprecation modifiers.
//! - `namespace`, `namespace::Colors`, `namespace::Colors::{Red, Green, Blue}`,
//!   and `namespace::Red` via a glob reexport of enum variants. The `namespace::Red` name is
//!   both a value (variant) and a type (struct), which is allowed across namespaces.
//!   `namespace::Green` and `namespace::Blue` are not exported because tuple/unit structs
//!   of the same names shadow the glob-imported variants in the value namespace.
//! - `namespace_glob_conflict`, `namespace_glob_conflict::Primary`,
//!   `namespace_glob_conflict::Secondary`, `namespace_glob_conflict::{Red, Cyan}`,
//!   and the enum-scoped variant paths.
//!   `namespace_glob_conflict::Green` and `namespace_glob_conflict::Blue` are not exported
//!   because two glob reexports introduce distinct variants with the same names.
//! - `value_shadow`, `value_shadow::Shadowed`, `value_shadow::Shadowed::Clash`.
//!   The glob import would also expose `value_shadow::Clash`, but it is shadowed by the
//!   unit struct `value_shadow::Clash` (a value + type name).
//! - `glob_same_item`, `glob_same_item::Source`,
//!   `glob_same_item::Source::{Same, SameTuple, SameStruct}`,
//!   and `glob_same_item::{Same, SameTuple, SameStruct}` via two glob reexports of the
//!   same underlying variants (no conflict because both globs point at the same items).
//! - `glob_conflict`, `glob_conflict::Left`, `glob_conflict::Right`,
//!   `glob_conflict::Left::{Clash, Tuple, Struct}`, `glob_conflict::Right::{Clash, Tuple, Struct}`,
//!   `glob_conflict::LeftOnly`, `glob_conflict::RightOnly`.
//!   `glob_conflict::{Clash, Tuple, Struct}` are not exported because two distinct variants
//!   conflict under overlapping glob reexports.

pub enum Base {
    Plain,
    #[deprecated]
    Deprecated,
    #[doc(hidden)]
    Hidden,
    #[deprecated]
    #[doc(hidden)]
    DeprecatedHidden,
}

pub use Base::*;
pub use Base::Plain as RenamedPlain;

#[doc(hidden)]
pub use Base::Plain as HiddenPlain;

#[deprecated]
pub use Base::Plain as DeprecatedPlain;

/// Proof that `Red` can be used as both a type and a value:
/// ```rust
/// use enum_variant_imports::namespace;
///
/// fn example(_r: namespace::Red) {
///     let _x: namespace::Colors = namespace::Red;
/// }
/// ```
///
/// `Green` is shadowed by the tuple struct with the same name:
/// ```compile_fail
/// use enum_variant_imports::namespace;
///
/// fn example(_g: namespace::Green) {
///     let _y: namespace::Colors = namespace::Green(42);
/// }
/// ```
///
/// `Blue` is shadowed by the unit struct with the same name:
/// ```compile_fail
/// use enum_variant_imports::namespace;
///
/// fn example() {
///     let _z: namespace::Colors = namespace::Blue { amount: 42 };
/// }
/// ```
pub mod namespace {
    pub enum Colors {
        Red,
        Green(u8),
        Blue { amount: u8 },
    }

    pub struct Red {
        pub value: u8,
    }

    pub struct Green(pub u8);

    pub struct Blue;

    pub use Colors::*;
}

#[allow(ambiguous_glob_reexports)]
pub mod namespace_glob_conflict {
    pub enum Primary {
        Red,
        Green(u8),
        Blue { amount: u8 },
    }

    pub enum Secondary {
        Green(u8),
        Blue { amount: u8 },
        Cyan,
    }

    pub use Primary::*;
    pub use Secondary::*;
}

pub mod value_shadow {
    pub enum Shadowed {
        Clash,
    }

    pub use Shadowed::*;

    pub struct Clash;
}

pub mod glob_same_item {
    pub enum Source {
        Same,
        SameTuple(u8),
        SameStruct { amount: u8 },
    }

    mod first {
        pub use super::Source::*;
    }

    mod second {
        pub use super::Source::*;
    }

    pub use first::*;
    pub use second::*;
}

#[allow(ambiguous_glob_reexports)]
pub mod glob_conflict {
    pub enum Left {
        Clash,
        Tuple(u8),
        Struct { amount: u8 },
        LeftOnly,
    }

    pub enum Right {
        Clash,
        Tuple(u8),
        Struct { amount: u8 },
        RightOnly,
    }

    pub use Left::*;
    pub use Right::*;
}
