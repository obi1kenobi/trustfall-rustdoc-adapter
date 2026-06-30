// Emit structured stdlib-style stability data by enabling unstable Rust features.
#![allow(internal_features)]
#![feature(const_trait_impl)]
#![feature(rustc_attrs)]
#![feature(staged_api)]
#![stable(feature = "rust_std_stability_fixture", since = "1.0.0")]

#[unstable(feature = "unstable_function", issue = "none")]
pub fn unstable_function() -> u32 {
    0
}

#[stable(feature = "stable_const_stable", since = "1.0.0")]
#[rustc_const_stable(feature = "const_stable", since = "1.0.0")]
pub const fn stable_const_stable() -> u32 {
    0
}

#[stable(feature = "stable_const_unstable", since = "1.0.0")]
#[rustc_const_unstable(feature = "const_unstable", issue = "none")]
pub const fn stable_const_unstable() -> u32 {
    0
}

#[unstable(feature = "unstable_const_function", issue = "none")]
pub const fn unstable_const_function() -> u32 {
    0
}

#[unstable(feature = "unstable_module", issue = "none")]
pub mod unstable_module {
    #[stable(feature = "stable_inside_unstable_module", since = "1.0.0")]
    pub fn stable_inside_unstable_module() -> u32 {
        0
    }
}

#[unstable(feature = "unstable_reexport_source", issue = "none")]
pub mod unstable_reexport_source {
    #[stable(feature = "stable_reexported_from_unstable_module", since = "1.0.0")]
    pub fn stable_reexported_from_unstable_module() -> u32 {
        0
    }
}

#[stable(feature = "stable_reexport_from_unstable_module", since = "1.0.0")]
pub use unstable_reexport_source::stable_reexported_from_unstable_module as stable_reexport_from_unstable_module;

#[stable(feature = "reexport_source", since = "1.0.0")]
pub mod reexport_source {
    #[stable(feature = "reexport_target", since = "1.0.0")]
    pub fn reexport_target() -> u32 {
        0
    }
}

#[stable(feature = "stable_reexport", since = "1.0.0")]
pub use reexport_source::reexport_target as stable_reexport_target;

#[unstable(feature = "unstable_reexport", issue = "none")]
pub use reexport_source::reexport_target as unstable_reexport_target;

#[stable(feature = "direct_glob_source", since = "1.0.0")]
pub mod direct_glob_source {
    #[stable(feature = "direct_glob_target", since = "1.0.0")]
    pub fn direct_glob_target() -> u32 {
        0
    }
}

#[unstable(feature = "unstable_direct_glob", issue = "none")]
pub use direct_glob_source::*;

#[stable(feature = "nested_outer", since = "1.0.0")]
pub mod nested_outer {
    #[stable(feature = "nested_inner", since = "1.0.0")]
    pub mod nested_inner {
        #[stable(feature = "nested_glob_target", since = "1.0.0")]
        pub fn nested_glob_target() -> u32 {
            0
        }
    }

    #[unstable(feature = "unstable_inner_glob", issue = "none")]
    pub use nested_inner::*;
}

#[stable(feature = "stable_outer_glob", since = "1.0.0")]
pub use nested_outer::*;

#[stable(feature = "non_glob_through_glob_source", since = "1.0.0")]
pub mod non_glob_through_glob_source {
    #[stable(feature = "non_glob_target", since = "1.0.0")]
    pub fn non_glob_target() -> u32 {
        0
    }

    #[unstable(feature = "non_glob_unstable_alias", issue = "none")]
    pub use self::non_glob_target as non_glob_unstable_alias;
}

#[stable(feature = "stable_glob_with_unstable_alias", since = "1.0.0")]
pub use non_glob_through_glob_source::*;

#[stable(feature = "item_stability_owner", since = "1.0.0")]
pub struct ItemStabilityOwner;

#[unstable(feature = "unstable_trait_with_unannotated_method", issue = "none")]
pub trait UnstableTraitWithUnannotatedMethod {
    // Rustdoc JSON should propagate the trait's instability onto this method item.
    // The adapter relies on that item-local `stability` instead of looking up the trait.
    fn unannotated_method_in_unstable_trait() -> u32;
}

#[unstable(feature = "unstable_inherent_impl", issue = "none")]
impl ItemStabilityOwner {
    // Rustdoc JSON should propagate the impl's instability onto this method item.
    // The adapter relies on that item-local `stability` instead of looking up the impl.
    pub fn method_inside_unstable_inherent_impl() -> u32 {
        0
    }
}

#[stable(feature = "const_impl_owner", since = "1.0.0")]
pub struct ConstImplOwner;

#[stable(feature = "const_inherent_impl", since = "1.0.0")]
#[rustc_const_unstable(feature = "const_inherent_impl_unstable", issue = "none")]
const impl ConstImplOwner {
    // Rustdoc JSON should propagate the impl's const-instability onto this method item.
    // The adapter relies on that item-local `const_stability` instead of looking up the impl.
    #[stable(feature = "const_impl_method", since = "1.0.0")]
    pub fn const_impl_method() -> u32 {
        0
    }
}

#[stable(feature = "fixture_const_trait", since = "1.0.0")]
#[rustc_const_unstable(feature = "fixture_const_trait_unstable", issue = "none")]
pub const trait FixtureConstTrait {
    // This is not syntactically `const`, but rustdoc JSON should still propagate
    // the trait's const-instability onto the method item.
    #[stable(feature = "provided", since = "1.0.0")]
    fn provided() {}
}

#[stable(feature = "fixture_const_bound", since = "1.0.0")]
#[rustc_const_unstable(feature = "fixture_const_bound_unstable", issue = "none")]
pub const trait FixtureConstBound {}

#[stable(feature = "const_trait_marker", since = "1.0.0")]
#[rustc_const_unstable(feature = "const_trait_marker_unstable", issue = "none")]
pub const fn const_trait_marker(
    arg: impl [const] FixtureConstBound,
) -> impl [const] FixtureConstBound {
    arg
}
