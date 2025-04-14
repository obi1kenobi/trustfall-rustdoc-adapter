use std::boxed::Box;
pub struct GenericStruct<T: ?Sized, U: Sized, V> {
    x: Box<T>,
    y: U,
    z: V,
}

pub enum GenericEnum<T: ?Sized, U: Sized, V> {
    Variant(Box<T>),
    Variant2(U),
    Variant3(V),
}

pub union GenericUnion<T: ?Sized + Copy, U: Sized, V> {
    field: std::mem::ManuallyDrop<Box<T>>,
    field2: std::mem::ManuallyDrop<U>,
    field3: std::mem::ManuallyDrop<V>,
}

pub trait GenericTrait<T: ?Sized, U: Sized, V> {
    fn method<W: ?Sized, X: Sized, Y>(value: &Box<T>);
}

pub fn generic_fn<T: ?Sized, U: Sized, V>(x: &T) {}
pub fn generic_fn_with_where_bound<T, U, V>(value: &T) where T: ?Sized, U: Sized {}

pub fn impl_trait<T: ?Sized, U: Sized, V>(value: impl GenericTrait<T, U, V>) {}