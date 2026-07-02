#![no_std]

pub struct ExampleStruct<T: Ord> {
    _marker: core::marker::PhantomData<T>,
}

pub enum ExampleEnum<T: PartialEq + Sync> {
    Variant(T),
}

pub union ExampleUnion<T: core::fmt::Debug + Copy> {
    value: T,
}

pub struct IteratorWrapper<T: Sync + Iterator<Item = i64>> {
    inner: T,
}

pub struct LifetimedIterator<'a, T: Iterator<Item = &'a str> + 'a> {
    inner: T,
}

pub struct SeparateIteratorBounds<'a, T>
where
    T: Iterator<Item = &'a str>,
    T: 'a
{
    inner: T,
}
