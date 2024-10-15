use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;

pub struct GenericStruct<'a, T: Clone + PartialOrd<i64>, const N: usize> {
    _marker: &'a PhantomData<[T; N]>,
}

pub enum GenericEnum<'a, T: Clone + PartialOrd<i64>, const N: usize> {
    Variant(&'a PhantomData<[T; N]>),
}

pub union GenericUnion<'a, T: Clone + PartialOrd<i64>, const N: usize> {
    field: &'a PhantomData<[T; N]>,
}

pub trait GenericTrait<'a, T: Clone + PartialOrd<i64>, const N: usize> {
    fn method<'b, U: Hash, const M: usize>(value: &'a PhantomData<&'b ([T; N], [U; M])>);
}

pub fn generic_fn<'a, T: Clone + PartialOrd<i64>, const N: usize>(x: &'a PhantomData<[T; N]>) {}

pub fn impl_trait<'a, T: Clone + PartialOrd<i64>, const N: usize>(
    value: impl GenericTrait<'a, T, N>,
) {
}

// Only the `T: Clone` bound should be present in `T`'s own type bounds.
// The `Arc<T>: Clone` bound involves `T` but isn't `T`'s own bound.
pub fn non_included_bound<T: Unpin>(value: T) where Arc<T>: Clone {}

// The generics here are equivalent to `<T: Iterator> where T::Item: Clone`,
// so `T: Iterator` should be `T`'s own type bound even though it's written in the `where` portion.
pub fn explicit_where_bound<T>(value: T) where T: Iterator, T::Item: Clone {}

pub struct DefaultGenerics<T: Copy = i64, const N: usize = 2> {
    value: [T; N],
}
