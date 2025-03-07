#![no_std]

#![allow(unused_variables)]

pub fn function<'a, 'b, T, U, const N: usize, const M: usize>(left: &'a [T; N], right: &'b [U; M]) {}

pub trait Trait<'a, T, const N: usize> {
    fn method<'b, U, V, const M: usize>(&self, value: &'b U) -> [V; M];
}

pub fn impl_trait<T, U>(first: T, impld: impl Into<U>) -> impl Iterator<Item = T> {}

pub struct Example<'a, 'b> {
    _marker: core::marker::PhantomData<&'a &'b ()>,
}

impl<'a, 'b> Example<'a, 'b> {
    pub fn elided_lifetimes<'c>(&self, arg: &'a i64, elided: &i64, explicit: &'c i64) -> &'_ i64 {
        todo!()
    }
}

pub struct SingleLifetimeElided<'a> {
    _marker: core::marker::PhantomData<&'a ()>,
}

impl SingleLifetimeElided<'_> {
    pub fn explicit_self_lifetime<'a>(&'a self, value: impl Into<&'a i64>) {}
}
