#![allow(internal_features)]
#![feature(const_trait_impl)]

pub const trait MaybeConst {}

pub trait Plain {}

pub const fn maybe_const_function_signature(
    arg: impl [const] MaybeConst,
) -> impl [const] MaybeConst {
    arg
}

pub const fn maybe_const_return_bound_const_then_plain(
    arg: impl [const] MaybeConst + Plain,
) -> impl [const] MaybeConst + Plain {
    arg
}

pub const fn maybe_const_return_bound_plain_then_const(
    arg: impl Plain + [const] MaybeConst,
) -> impl Plain + [const] MaybeConst {
    arg
}
