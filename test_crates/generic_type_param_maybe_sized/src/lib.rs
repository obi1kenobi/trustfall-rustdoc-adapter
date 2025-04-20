use std::boxed::Box;
pub struct GenericStruct<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    x: Box<T>,
    y: U,
    z: V,
}

pub enum GenericEnum<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    Variant(Box<T>),
    Variant2(U),
    Variant3(V),
}

pub union GenericUnion<T: ?core::marker::Sized + Copy, U: core::marker::Sized, V> {
    field: std::mem::ManuallyDrop<Box<T>>,
    field2: std::mem::ManuallyDrop<U>,
    field3: std::mem::ManuallyDrop<V>,
}

pub trait GenericTrait<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    fn method<W: ?core::marker::Sized, X: core::marker::Sized, Y>(value: &Box<T>);
}

pub fn generic_fn1<T: ?core::marker::Sized, U: core::marker::Sized, V>(x: &T) {}

pub trait Sized { fn method(); }

pub fn generic_fn2<T: core::marker::Sized + ?core::marker::Sized>(value: &T) {}

pub fn generic_fn3<T: Sized + ?core::marker::Sized>(value: T) {}

pub fn generic_fn4<T: core::marker::Sized + ?core::marker::Sized>(value: &T) where T: core::marker::Sized {}

pub fn generic_fn5<T: ?core::marker::Sized + core::marker::Sized>(value: &T) where T: core::marker::Sized {}

pub fn generic_fn6<T: ?core::marker::Sized>(value: &T) where T: core::marker::Sized {}

pub fn generic_fn7<T: core::marker::Sized>(value: &T) where T: ?core::marker::Sized {}

pub fn generic_fn8<T, U, V>(value: &T) where T: ?core::marker::Sized, U: core::marker::Sized {}

pub fn impl_trait<T: ?core::marker::Sized, U: core::marker::Sized, V>(value: impl GenericTrait<T, U, V>) {}

