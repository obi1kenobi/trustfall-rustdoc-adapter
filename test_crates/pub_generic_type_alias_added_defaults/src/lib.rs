//! This package exports the following:
//! - `inner::Foo`
//! - `inner::Bar`
//! - `DefaultFoo`
//! - `DefaultBar`
//!
//! Adding a default generic parameter value in the typedef makes it no longer
//! equivalent to the underlying, and not equivalent to a `pub use`.
//! It cannot be treated as a re-export of the underlying type.

pub mod inner {
    pub struct Foo<'a, T, const N: usize> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }

    pub struct Bar<'a, const N: usize, T> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }
}

pub type DefaultFoo<'a, T, const N: usize = 5> = crate::inner::Foo<'a, T, N>;

pub type DefaultBar<'a, const N: usize, T = i64> = crate::inner::Bar<'a, N, T>;
