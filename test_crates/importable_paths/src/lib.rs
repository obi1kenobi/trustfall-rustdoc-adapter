pub struct PublicImportable {}

mod private {
    pub struct PubInPriv {}

    struct Private {}
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

// Our doc-hidden analysis works even when `#[doc(hidden)]` does not appear verbatim
// in the attributes, and is instead combined with other `doc` commands.
#[doc(hidden, alias = "TheAlias")]
pub struct Aliased;
