//! Using `#[target_feature]` on functions and methods.
//!
//! Certain combinations don't compile.
//!
//! - Having whitespace in the feature list:
//! ```compile_fail
//! #[target_feature(enable = "sse2, avx")]
//! pub fn features_with_spaces() {}
//! ```
//!
//! - `#[target_feature]` featuring unrecognized features
//! ```compile_fail
//! pub trait Trait {
//!     #[target_feature(enable = "unrecognized")]
//!     fn non_defaulted_method();
//! }
//! ```
//!
//! - `#[target_feature]` on safe trait associated functions
//! ```compile_fail
//! pub trait Trait {
//!     #[target_feature(enable = "sse2")]
//!     fn safe_trait_method() {}
//! }
//! ```
//!
//! - `#[target_feature]` on trait associated functions without default impls
//! ```compile_fail
//! pub trait Trait {
//!     #[target_feature(enable = "sse2")]
//!     fn non_defaulted_method();
//! }
//! ```

#[target_feature(enable = "sse2,avx")]
pub fn top_level_fn() {}

#[target_feature(enable = "sse2,avx,avx2")]
pub unsafe fn unsafe_top_level_fn() {}

#[target_feature(enable = "sse2,avx2")]
pub fn implies_avx() {}

pub struct Example;

impl Example {
    #[target_feature(enable = "sse2,avx,avx2")]
    pub fn struct_method() {}

    #[target_feature(enable = "sse2")]
    pub unsafe fn unsafe_struct_method() {}
}

pub trait Trait {
    #[target_feature(enable = "sse2")]
    unsafe fn defaulted_trait_method() {}
}

impl Trait for Example {
    // Enable more features than the trait described.
    #[target_feature(enable = "sse2,avx2")]
    unsafe fn defaulted_trait_method() {}
}
