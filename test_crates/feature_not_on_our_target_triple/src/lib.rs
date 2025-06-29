#![feature(sparc_target_feature)]  // requires nightly
#![no_std]

// If we are running on common x86 and ARM platforms like
// `x86_64-unknown-linux-gnu` or `aarch64-apple-darwin`,
// the `leoncasa` feature is not available.
//
// That renders these function unusable on those platforms,
// since the target requirement cannot be satisfied.

#[target_feature(enable = "leoncasa")]
pub fn safe_fn() {}

#[target_feature(enable = "leoncasa")]
pub unsafe fn unsafe_fn() {}

// It's impossible to satisfy both of these features simultaneously.
// No such hardware exists, to my knowledge.
#[target_feature(enable = "leoncasa,avx2")]
pub unsafe fn impossible_to_satisfy() {}
