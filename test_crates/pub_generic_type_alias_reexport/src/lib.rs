//! This package exports the following renames of `Foo` (which itself is not directly visible):
//! - `Exported<'a, T, const N: usize>`
//! - `ExportedRenamedParams<'b, U, const M: usize>`
//!
//! The following type aliases are also exported but do not qualify
//! as renames since they do not leave Foo's generic parameters unmodified:
//! - `ExportedWithDefaults<'a, T = i64, const N: usize = 5>`
//! - `ExportedSpecificLifetime<T, const N: usize>`
//! - `ExportedSpecificType<'a, const N: usize>`
//! - `ExportedSpecificConst<'a, T>`
//! - `ExportedFullySpecified`
//!
//! The `inner` module is private so `Foo` is not exported.
//! However, all type aliases are public and are exported.
//! The `Foo::bar` function is also visible, via the renames of `Foo`.
//!
//! As far as anyone outside this crate can tell,
//! the `Foo` type is actually called `Exported` or `ExportedRenamedParams` now.

mod inner {
    pub struct Foo<'a, T, const N: usize> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }

    impl<'a, T, const N: usize> Foo<'a, T, N> {
        pub fn bar() {}
    }
}

// The following type aliases are valid re-exports.
// They do not change the meaning of the generic parameters, even while renaming them
// or supplying default values.

pub type Exported<'a, T, const N: usize> = crate::inner::Foo<'a, T, N>;

pub type ExportedRenamedParams<'b, U, const M: usize> = crate::inner::Foo<'b, U, M>;

// This type alias is not a re-export, since it adds default values for the generic parameters.
// This is not achievable with a `pub use`, so it cannot be a re-export.
pub type ExportedWithDefaults<'a, T = i64, const N: usize = 5> = crate::inner::Foo<'a, T, N>;

// The following type aliases are not valid re-exports,
// since they constrain the underlying type's generic parameters.

pub type ExportedSpecificLifetime<T, const N: usize> = crate::inner::Foo<'static, T, N>;

pub type ExportedSpecificType<'a, const N: usize> = crate::inner::Foo<'a, i64, N>;

pub type ExportedSpecificConst<'a, T> = crate::inner::Foo<'a, T, 5>;

pub type ExportedFullySpecified = crate::inner::Foo<'static, i64, 5>;

// Bounds on type aliases are not checked.
// The following line triggers the rustc `type_alias_bounds` warning,
// and rustc suggests removing the bound.
//
// It's plausible that in the future, this may become a hard error,
// so we won't mandate a particular behavior on it in our library.
// ```
// pub type ExportedWithWhereBound<'a, T: Into<i64>, const N: usize> = crate::inner::Foo<'a, T, N>;
// ```
