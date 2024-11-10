use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;
use std::io::Write as IoWrite;

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

// The generics here are equivalent to `<T: Iterator>`,
// so `T: Iterator` should be `T`'s own type bound even though it's written in the `where` portion.
pub fn explicit_where_bound<T>(value: T) where T: Iterator {}

// The generics here are equivalent to `<T: Clone + Iterator>`,
// both bounds should be `T`'s own type bounds even though they are written in the `where` portion.
pub fn combined_explicit_where_bound<T>(value: T) where T: Iterator, T: Clone {}

// The generics here are equivalent to `<T: Iterator> where T::Item: Clone`,
// so `T: Iterator` should be `T`'s own type bound even though it's written in the `where` portion.
pub fn complex_explicit_where_bound<T>(value: T) where T: Iterator, T::Item: Clone {}

// The generics here are equivalent to `<T: Clone + Iterator>`, or equivalently,
// `<T> where T: Clone, T: Iterator` so bounds should be `T`'s own type bounds
// regardless of where they appear.
pub fn combined_bounds<T: Clone>(value: T) where T: Iterator {}

// We expect the `ImplementedTrait` in the bound here to still have name `Debug`,
// even though locally we're using a full path instead of an import.
pub fn full_path_trait_bound<T: std::fmt::Debug>(value: T) {}

// We expect the `ImplementedTrait` in the bound here to still have name `Write`,
// even though we've locally renamed the import to `IoWrite`.
pub fn renamed_trait_bound<T: IoWrite>(value: T) {}

// Default values for generic parameters should be observed and reported.
pub struct DefaultGenerics<T: Copy = i64, const N: usize = 2> {
    value: [T; N],
}
