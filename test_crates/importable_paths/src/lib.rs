#![no_std]

pub struct PublicImportable {}

mod private {
    pub struct PubInPriv {}

    struct Private {}

    enum PrivateEnum {
        NotHidden,

        #[deprecated]
        Deprecated,

        #[deprecated]
        #[doc(hidden)]
        DeprecatedHidden,

        #[doc(hidden)]
        Hidden,
    }

    union PrivateUnion {
        foo: usize
    }

    trait SomeTrait {
        #[doc(hidden)]
        #[deprecated]
        type T;

        #[doc(hidden)]
        #[deprecated]
        const N: i64;

        #[doc(hidden)]
        #[deprecated]
        fn associated();
    }
}

#[doc(hidden)]
pub mod hidden {
    pub struct ModuleHidden {}

    #[deprecated]
    pub struct DeprecatedModuleHidden {} // public_api

    #[deprecated]
    pub mod deprecated {
        pub struct ModuleDeprecatedModuleHidden {} // public_api
    }
}

pub mod submodule {
    #[doc(hidden)]
    pub struct Hidden {}

    #[deprecated]
    #[doc(hidden)]
    pub struct DeprecatedHidden {} // public_api
}

#[deprecated]
pub mod deprecated {
    pub struct ModuleDeprecated {} // public_api

    #[doc(hidden)]
    pub struct ModuleDeprecatedHidden {} // public_api
}

// This is expected to be visible in rustdoc.
pub use hidden::ModuleHidden as UsedVisible; // public_api

// This is expected to be hidden in rustdoc.
pub use submodule::Hidden as UsedHidden;

// This is expected to be public_api and deprecated
pub use deprecated::ModuleDeprecated as UsedModuleDeprecated;

// Still public_api, the item is deprecated (via its module) so the item is visible.
pub use deprecated::ModuleDeprecatedHidden as UsedModuleDeprecatedHidden;

pub mod reexports {
    // Re-exports can be deprecated too.
    #[deprecated]
    pub use super::PublicImportable as DeprecatedReexport;

    // Re-exports can be doc-hidden as well.
    #[doc(hidden)]
    pub use super::PublicImportable as HiddenReexport;

    // Doc-hidden re-exports of deprecated items are still public API.
    #[doc(hidden)]
    pub use super::deprecated::ModuleDeprecated as HiddenDeprecatedReexport;
}

mod hidden_glob_source {
    pub struct HiddenGlobOnly;
    pub struct BothHiddenAndVisible;
    pub struct BothHiddenAndVisibleSameName;
}

// Items visible through here are not public API.
#[doc(hidden)]
pub use hidden_glob_source::*;

// This name is public API though.
pub use hidden_glob_source::BothHiddenAndVisible as VisibleBothHiddenAndVisible;

// This name is also public API, even though it shadows the same (non-public API) name
// as the one from the `doc(hidden)` glob re-export.
pub use hidden_glob_source::BothHiddenAndVisibleSameName;

mod duplicate_glob_source {
    pub struct DuplicateGlobHiddenAndVisible;
}

// If multiple glob re-exports make available the same item, then the item's path is public API
// if *either* of the re-exports is public API. The `doc(hidden)` re-export does not matter.
#[doc(hidden)]
pub use duplicate_glob_source::*;
pub use duplicate_glob_source::*;

mod duplicate_hidden_deprecated_glob_source {
    pub struct DuplicateGlobHiddenDeprecatedAndVisible;
}

// The same rule applies if the hidden re-export is also deprecated. The regular re-export
// gives downstream users a non-hidden, non-deprecated path to the same item.
#[deprecated]
#[doc(hidden)]
pub use duplicate_hidden_deprecated_glob_source::*;
pub use duplicate_hidden_deprecated_glob_source::*;

mod duplicate_deprecated_glob_source {
    pub struct DuplicateGlobDeprecatedAndVisible;
}

// The same rule applies if one re-export is deprecated. The regular re-export gives
// downstream users a non-deprecated path to the same item.
#[deprecated]
pub use duplicate_deprecated_glob_source::*;
pub use duplicate_deprecated_glob_source::*;

mod deprecated_glob_source {
    pub struct DeprecatedGlobOnly;
}

#[deprecated]
pub use deprecated_glob_source::*;

mod hidden_deprecated_glob_source {
    pub struct HiddenDeprecatedGlobOnly;
}

// Everything re-exported here is public API because of `#[deprecated]`.
#[deprecated]
#[doc(hidden)]
pub use hidden_deprecated_glob_source::*;

mod nested_glob_source {
    pub struct NestedHiddenGlobOnly;
}

mod nested_glob_layer {
    #[doc(hidden)]
    pub use super::nested_glob_source::*;
}

// Everything re-exported here is public API.
// It doesn't matter that the names within the module
// are imported with `#[doc(hidden)]`.
// The external user is not relying on those -- they are using a public API item only.
pub use nested_glob_layer::*;

mod nested_hidden_deprecated_glob_source {
    pub struct NestedHiddenDeprecatedGlobOnly;
}

mod nested_hidden_deprecated_glob_layer {
    #[deprecated]
    #[doc(hidden)]
    pub use super::nested_hidden_deprecated_glob_source::*;
}

// Everything re-exported here is public API.
// It doesn't matter that the names within the module
// are imported with `#[doc(hidden)]` nor `#[deprecated]`.
// The external user is not relying on those -- they are using a public API item only.
pub use nested_hidden_deprecated_glob_layer::*;

mod plain_glob_source {
    pub struct PlainGlobOnly;
}

pub use plain_glob_source::*;

// A non-hidden re-export of an internally-hidden re-export is public API.
//
// For this next batch of re-exports, the fact that the intermediate re-export
// may be `#[doc(hidden)]` doesn't matter. Items can be made to be non-public API
// if *either* the item's definition itself is `#[doc(hidden)]`
// *or* if the path that an external (downstream) user might type involves a `#[doc(hidden)]` name,
// in each case also accounting for `#[deprecated]` exceptions of course.

mod top_level_public_api_per_item_sources {
    pub struct HiddenPerItemThenRootPerItem;
    pub struct HiddenPerItemThenRootGlob;
}

mod hidden_glob_then_root_per_item_source {
    pub struct HiddenGlobThenRootPerItem;
}

mod hidden_glob_then_root_glob_source {
    pub struct HiddenGlobThenRootGlob;
}

mod hidden_per_item_for_root_per_item {
    #[doc(hidden)]
    pub use super::top_level_public_api_per_item_sources::HiddenPerItemThenRootPerItem;
}

pub use hidden_per_item_for_root_per_item::HiddenPerItemThenRootPerItem;

mod hidden_per_item_for_root_glob {
    #[doc(hidden)]
    pub use super::top_level_public_api_per_item_sources::HiddenPerItemThenRootGlob;
}

pub use hidden_per_item_for_root_glob::*;

mod hidden_glob_for_root_per_item {
    #[doc(hidden)]
    pub use super::hidden_glob_then_root_per_item_source::*;
}

pub use hidden_glob_for_root_per_item::HiddenGlobThenRootPerItem;

mod hidden_glob_for_root_glob {
    #[doc(hidden)]
    pub use super::hidden_glob_then_root_glob_source::*;
}

pub use hidden_glob_for_root_glob::*;

// Our doc-hidden analysis works even when `#[doc(hidden)]` does not appear verbatim
// in the attributes, and is instead combined with other `doc` commands.
#[doc(hidden, alias = "TheAlias")]
pub struct Aliased;
