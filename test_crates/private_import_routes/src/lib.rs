#![no_std]
#![expect(
    unused_imports,
    reason = "We're testing name resolution in the presence of private imports."
)]

mod named_source {
    pub struct PrivateNamed;
    pub struct CrateNamed;
    pub struct SuperNamed;
    pub struct PublicNamed;
}

pub mod named_imports {
    use super::named_source::PrivateNamed;

    pub(crate) use super::named_source::CrateNamed;

    pub(super) use super::named_source::SuperNamed;

    pub use super::named_source::PublicNamed;
}

pub mod named_chain {
    pub use super::named_imports::*;
}

mod alias_source {
    pub struct Aliased;
}

use alias_source as private_alias;
pub use private_alias::Aliased as PublicAlias;

mod private_glob_source {
    pub struct PrivateGlob;
}

pub mod private_glob {
    use super::private_glob_source::*;
}

pub mod private_glob_chain {
    pub use super::private_glob::*;
}

mod crate_glob_source {
    pub struct CrateGlob;
}

pub mod crate_glob {
    pub(crate) use super::crate_glob_source::*;
}

pub mod crate_glob_chain {
    pub use super::crate_glob::*;
}

mod super_glob_source {
    pub struct SuperGlob;
}

pub mod super_glob {
    pub(super) use super::super_glob_source::*;
}

pub mod super_glob_chain {
    pub use super::super_glob::*;
}

mod enum_source {
    pub enum PrivatelyImportedEnum {
        PrivateVariant,
    }

    pub enum PubliclyReexportedEnum {
        PublicVariant,
    }
}

pub mod private_enum_glob {
    use super::enum_source::PrivatelyImportedEnum::*;
}

pub mod private_enum_glob_chain {
    pub use super::private_enum_glob::*;
}

pub mod public_enum_glob {
    pub use super::enum_source::PubliclyReexportedEnum::*;
}

mod shared_source {
    pub struct Shared;
}

pub mod shared_private {
    use super::shared_source::*;
}

pub mod shared_public {
    pub use super::shared_source::*;
}

pub mod private_outer {
    use super::shared_public::*;
}

pub mod shared_routes {
    pub use super::shared_private::*;
    pub use super::shared_public::*;
}

pub mod shared_direct {
    use super::shared_source::*;
    pub use super::shared_source::*;
}

mod hidden_source {
    pub struct HiddenShared;
}

pub mod hidden_routes {
    use super::hidden_source::*;

    #[doc(hidden)]
    pub use super::hidden_source::*;
}

pub mod hidden_chain {
    pub use super::hidden_routes::*;
}

mod ambiguity_source {
    pub struct PrivateCandidate;
    pub struct PublicCandidate;
}

mod private_candidate {
    pub use super::ambiguity_source::PrivateCandidate as Collision;
}

mod public_candidate {
    pub use super::ambiguity_source::PublicCandidate as Collision;
}

pub mod ambiguous_globs {
    use super::private_candidate::*;
    pub use super::public_candidate::*;
}

mod ancestor_source {
    pub struct AncestorPrivate;
    pub struct AncestorPublic;
}

mod ancestor_public_candidate {
    pub use super::ancestor_source::AncestorPublic as AncestorCollision;
}

pub mod ancestor {
    use super::ancestor_source::AncestorPrivate as AncestorCollision;

    pub mod descendant {
        pub use super::*;
        #[allow(
            ambiguous_glob_reexports,
            reason = "Compilers diagnose this intentional ancestor-name conflict with different lints."
        )]
        pub use crate::ancestor_public_candidate::*;
    }
}
