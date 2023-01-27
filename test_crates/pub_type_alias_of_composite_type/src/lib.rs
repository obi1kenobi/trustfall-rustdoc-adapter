//! None of the `pub type` uses here are equivalent to a `pub use`,
//! so all the types and type aliases here are individually exported.

pub mod inner {
    pub struct Foo<'a, T, const N: usize> {
        _marker: std::marker::PhantomData<&'a T>,
        _n: [i64; N],
    }
}

pub type I64Tuple = (i64, i64);

pub type MixedTuple = (i64, inner::Foo<'static, bool, 5>);

pub type GenericTuple<T> = (inner::Foo<'static, T, 5>, i64);

pub type LifetimeTuple<'a> = (inner::Foo<'a, bool, 5>, i64);

pub type ConstTuple<const N: usize> = (inner::Foo<'static, bool, N>, i64);

pub type DefaultGenericTuple<T = String> = (inner::Foo<'static, T, 5>, i64);

pub type DefaultConstTuple<const N: usize = 7> = (inner::Foo<'static, bool, N>, i64);
