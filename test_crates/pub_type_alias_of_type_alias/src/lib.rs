//! This package exports the following:
//! - `inner::Foo`, also as `inner::AliasedFoo` and `ExportedFoo`
//! - `inner::Bar`, also as `inner::AliasedBar` and `ExportedBar`
//! - `inner::AliasedFoo`, also as `ExportedFoo`
//! - `inner::AliasedBar`, also as `ExportedBar`
//! - `ExportedFoo`
//! - `ExportedBar`
//! - `DifferentLifetimeBar`
//! - `DifferentGenericBar`
//! - `DifferentConstBar`
//! - `ReorderedBar`
//! - `DefaultValueBar`

pub mod inner {
    pub struct Foo;

    pub struct Bar<'a, T, const N: usize> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }

    pub type AliasedFoo = Foo;
    pub type AliasedBar<'a, T, const N: usize> = Bar<'a, T, N>;
}

pub type ExportedFoo = crate::inner::AliasedFoo;
pub type ExportedBar<'a, T, const N: usize> = crate::inner::AliasedBar<'a, T, N>;

// The following type aliases change the generics relative to `AliasedBar`, and are not re-exports.

pub type DifferentLifetimeBar<T, const N: usize> = crate::inner::AliasedBar<'static, T, N>;
pub type DifferentGenericBar<'a, const N: usize> = crate::inner::AliasedBar<'a, i64, N>;
pub type DifferentConstBar<'a, T> = crate::inner::AliasedBar<'a, T, 5>;
pub type ReorderedBar<'a, const T: usize, N> = crate::inner::AliasedBar<'a, N, T>;
pub type DefaultValueBar<'a, T, const N: usize = 7> = crate::inner::AliasedBar<'a, T, N>;
