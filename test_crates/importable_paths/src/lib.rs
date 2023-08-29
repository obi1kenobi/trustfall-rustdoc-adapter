pub struct PublicImportable {}

mod private {
    pub struct Private {}
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

// not public_api; it's not relevant that the module was deprecated
pub use deprecated::ModuleDeprecatedHidden as UsedModuleDeprecatedHidden;
