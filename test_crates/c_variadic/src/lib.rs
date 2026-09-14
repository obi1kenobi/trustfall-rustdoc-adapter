#![no_std]
#![allow(stable_features)]
#![feature(c_variadic)]

unsafe extern "C" {
    pub fn declared_unsafe(count: i32, ...);
    pub safe fn declared_safe(...);
    pub fn declared_non_variadic(value: i32);
}

unsafe extern "C-unwind" {
    pub fn declared_unwind(count: i32, ...);
}

pub unsafe extern "C" fn defined_named(count: i32, _args: ...) -> i32 {
    count
}

pub unsafe extern "C" fn defined_unnamed(count: i32, _: ...) {
    let _ = count;
}

pub unsafe extern "C" fn defined_no_fixed_params(_: ...) {}

pub unsafe extern "C-unwind" fn defined_unwind(_args: ...) {}

pub extern "C" fn non_variadic_c(value: i32) {
    let _ = value;
}

pub fn non_variadic_rust(value: i32) {
    let _ = value;
}

pub fn takes_variadic_pointer(callback: unsafe extern "C" fn(i32, ...)) {
    let _ = callback;
}

#[repr(C)]
pub struct Variadic {
    pub value: i32,
}

impl Variadic {
    pub unsafe extern "C" fn inherent_variadic(_: ...) {}

    pub unsafe extern "C" fn variadic_receiver(&self, _: ...) {}

    pub fn inherent_non_variadic() {}
}

pub trait VariadicTrait {
    unsafe extern "C" fn required_variadic(count: i32, _: ...);

    unsafe extern "C-unwind" fn default_variadic(_: ...) {}

    fn non_variadic() {}
}

impl VariadicTrait for Variadic {
    unsafe extern "C" fn required_variadic(count: i32, _: ...) {
        let _ = count;
    }
}
