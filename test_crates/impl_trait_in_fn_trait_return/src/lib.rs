#![feature(impl_trait_in_fn_trait_return)]

pub trait Zed {}
impl<T> Zed for T {}

pub fn fn_bound_opaque_return() -> impl Fn() -> (impl Clone + Copy) + Clone {
    || 1u8
}

pub fn fn_bound_opaque_return_first() -> impl Fn() -> (impl Clone + Copy) + Zed {
    || 1u8
}
