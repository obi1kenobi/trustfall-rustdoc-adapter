#![allow(internal_features)]
#![feature(const_trait_impl)]

pub const trait MaybeConst {}

pub const fn maybe_const_function_signature(
    arg: impl [const] MaybeConst,
) -> impl [const] MaybeConst {
    arg
}
