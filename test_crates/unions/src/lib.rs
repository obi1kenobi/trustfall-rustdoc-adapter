// Unions have their own special test suite because they were added after structs
// and enums.
//
// Part 1: unions import correctly. Largely derived from the importable paths
// test crate.

pub union PublicImportable {
    x: usize,
}

mod private {
    pub union PubInPriv {
        x: usize,
    }

    union Private {
        x: usize,
    }

    union PrivateUnion {
        foo: usize,
    }
}

#[doc(hidden)]
pub mod hidden {
    pub union ModuleHidden {
        x: usize,
    }

    #[deprecated]
    pub union DeprecatedModuleHidden {
        x: usize,
    } // public_api

    #[deprecated]
    pub mod deprecated {
        pub union ModuleDeprecatedModuleHidden {
            x: usize,
        } // public_api
    }
}

pub mod submodule {
    #[doc(hidden)]
    pub union Hidden {
        x: usize,
    }

    #[deprecated]
    #[doc(hidden)]
    pub union DeprecatedHidden {
        x: usize,
    } // public_api
}

#[deprecated]
pub mod deprecated {
    pub union ModuleDeprecated {
        x: usize,
    } // public_api

    #[doc(hidden)]
    pub union ModuleDeprecatedHidden {
        x: usize,
    } // public_api
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

// Part 2: union data/info querying is correct

mod data {
    use std::mem::ManuallyDrop;

    pub union NoFieldsPublic {
        x: usize,
        y: f32,
    }

    pub union SomeFieldsPublic {
        x: usize,
        pub y: f32,
    }

    pub union AllFieldsPublic {
        pub x: usize,
        pub y: f32,
    }
}
