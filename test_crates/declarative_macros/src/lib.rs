/// Usable externally as:
/// ```rust
/// declarative_macros::top_level!();
/// ```
///
/// Also usable with `#[macro_use]`:
/// ```rust
/// #[macro_use] extern crate declarative_macros;
///
/// top_level!();
/// ```
#[macro_export]
macro_rules! top_level {
    () => {}
}

mod private {
    /// Usable at the top level of the crate, due to `#[macro_export]`:
    /// ```rust
    /// declarative_macros::nested_private!();
    /// ```
    ///
    /// Also usable with `#[macro_use]`:
    /// ```rust
    /// #[macro_use] extern crate declarative_macros;
    ///
    /// nested_private!();
    /// ```
    ///
    /// But the following doesn't work due to the fact that exported macros
    /// are always exported at the root of the crate. Besides, this module is private
    /// so that path is not accessible outside the crate in general.
    /// ```rust,compile_fail
    /// declarative_macros::private::nested_private!();
    /// ```
    #[macro_export]
    macro_rules! nested_private {
        () => {}
    }
}

pub mod public {
    /// Usable at the top level of the crate, due to `#[macro_export]`:
    /// ```rust
    /// declarative_macros::nested_public!();
    /// ```
    ///
    /// Also usable with `#[macro_use]`:
    /// ```rust
    /// #[macro_use] extern crate declarative_macros;
    ///
    /// nested_public!();
    /// ```
    ///
    /// But exported macros are always exported at the crate root,
    /// not at the path where they happend to be defined. So the following doesn't work:
    /// ```rust,compile_fail
    /// extern crate declarative_macros;
    ///
    /// declarative_macros::public::nested_public!();
    /// ```
    #[macro_export]
    macro_rules! nested_public {
        () => {}
    }
}

/// Macros without `#[macro_export]` are not exported nor visible outside the crate:
/// ```rust,compile_fail
/// declarative_macros::not_exported!();
/// ```
///
/// They aren't usable with explicit `#[macro_use]` either:
/// ```rust,compile_fail
/// #[macro_use] extern crate declarative_macros;
///
/// not_exported!();
/// ```
#[allow(unused_macros)]
macro_rules! not_exported {
    () => {}
}

#[doc(hidden)]
pub mod hidden {
    /// This macro is *not* hidden, even though its defining module is hidden.
    /// This is because `#[macro_export]` behaves as a `pub use` of the macro item
    /// at the crate's top level.
    ///
    /// Usable at the top level of the crate, due to `#[macro_export]`:
    /// ```rust
    /// declarative_macros::hidden_parent!();
    /// ```
    ///
    /// Also usable with `#[macro_use]`:
    /// ```rust
    /// #[macro_use] extern crate declarative_macros;
    ///
    /// hidden_parent!();
    /// ```
    #[macro_export]
    macro_rules! hidden_parent {
        () => {}
    }
}

/// This macro is hidden since `#[doc(hidden)]` appears on it directly.
/// The `#[macro_export]` behaves as a `pub use` of the macro item
/// at the crate's top level, but the item itself is hidden so the end result is hidden.
///
/// Usable at the top level of the crate, due to `#[macro_export]`:
/// ```rust
/// declarative_macros::hidden!();
/// ```
///
/// Also usable with `#[macro_use]`:
/// ```rust
/// #[macro_use] extern crate declarative_macros;
///
/// hidden!();
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! hidden {
    () => {}
}
