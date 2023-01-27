//! This package does *not* include any re-exports of `inner::Foo` and `inner::Bar`.
//!
//! All type aliases change the semantics of the underlying type's generic parameters,
//! which disqualifies them from being equivalent to re-exports. This is due to:
//! - omitting default values for generic parameters
//! - changing default values for generic parameters
//!
//! All type aliases are public and are exported.

pub mod inner {
    pub struct Foo<'a, T = i64, const N: usize = 5> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }

    pub struct Bar<'a, const N: usize = 5, T = i64> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }
}

pub type ExportedWithoutTypeDefault<'a, T, const N: usize = 5> = crate::inner::Foo<'a, T, N>;

// This uses `Bar` instead of `Foo` because generic parameters with defaults must be trailing.
pub type ExportedWithoutConstDefault<'a, const N: usize, T = i64> = crate::inner::Bar<'a, N, T>;

pub type ExportedWithoutDefaults<'a, T, const N: usize> = crate::inner::Foo<'a, T, N>;

pub type ExportedWithDifferentTypeDefault<'a, T = usize, const N: usize = 5> =
    crate::inner::Foo<'a, T, N>;

pub type ExportedWithDifferentConstDefault<'a, T = i64, const N: usize = 10> =
    crate::inner::Foo<'a, T, N>;

pub type ExportedWithDifferentDefaults<'a, T = usize, const N: usize = 10> =
    crate::inner::Foo<'a, T, N>;
