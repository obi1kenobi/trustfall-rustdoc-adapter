#![allow(dead_code, unused_imports)]
#![deny(ambiguous_glob_imports)]

pub mod private_sibling {
    mod source {
        pub struct PrivateCandidate;
    }
    mod hidden {
        use super::source::PrivateCandidate as PrivateExport;
    }
    mod exported {
        pub struct PrivateExport;
    }

    pub use exported::*;
    pub use hidden::*;
}

pub mod super_sibling {
    mod source {
        pub struct SuperCandidate;
    }
    mod hidden {
        pub mod inner {
            pub(super) use super::super::source::SuperCandidate as SuperExport;
        }
    }
    mod exported {
        pub struct SuperExport;
    }

    pub use exported::*;
    pub use hidden::inner::*;
}

pub mod restricted_sibling {
    mod source {
        pub struct RestrictedCandidate;
    }
    mod hidden {
        pub mod inner {
            pub mod deeper {
                pub(in crate::restricted_sibling::hidden) use super::super::super::source::RestrictedCandidate as RestrictedExport;
            }
        }
    }
    mod exported {
        pub struct RestrictedExport;
    }

    pub use exported::*;
    pub use hidden::inner::deeper::*;
}

#[allow(ambiguous_glob_reexports)]
pub mod private_descendant {
    mod source {
        pub struct AncestorCandidate;
    }
    mod exported {
        pub struct AncestorExport;
    }
    mod hidden {
        use super::source::AncestorCandidate as AncestorExport;
        pub mod inner {
            pub use super::super::exported::*;
            pub use super::*;
        }
    }

    pub use hidden::inner::*;
}

#[allow(ambiguous_glob_reexports)]
pub mod super_accessible {
    mod source {
        pub struct AccessibleSuperCandidate;
    }
    mod exported {
        pub struct AccessibleSuperExport;
    }
    mod hidden {
        mod restricted {
            pub(super) use super::super::source::AccessibleSuperCandidate as AccessibleSuperExport;
        }
        pub use super::exported::*;
        pub use restricted::*;
    }

    pub use hidden::*;
}

#[allow(ambiguous_glob_reexports)]
pub mod restricted_accessible {
    mod source {
        pub struct AccessibleRestrictedCandidate;
    }
    mod exported {
        pub struct AccessibleRestrictedExport;
    }
    mod hidden {
        mod intermediate {
            pub mod restricted {
                pub(in crate::restricted_accessible::hidden) use super::super::super::source::AccessibleRestrictedCandidate as AccessibleRestrictedExport;
            }
        }
        pub mod inner {
            pub use super::super::exported::*;
            pub use super::intermediate::restricted::*;
        }
    }

    pub use hidden::inner::*;
}

pub mod local_shadow {
    mod source {
        pub struct ShadowCandidate;
    }
    mod exported {
        pub struct ShadowExport;
    }
    mod hidden {
        pub use super::exported::*;
        use super::source::ShadowCandidate as ShadowExport;
    }

    pub use hidden::*;
}

pub mod nested_inaccessible {
    mod source {
        pub struct NestedCandidate;
    }
    mod hidden {
        use super::source::NestedCandidate as NestedExport;
    }
    mod exported {
        pub struct NestedExport;
    }
    mod middle {
        pub use super::exported::*;
        pub use super::hidden::*;
    }

    pub use middle::*;
}

pub mod same_item {
    mod source {
        pub struct Shared;
    }
    mod hidden {
        pub(crate) use super::source::Shared;
    }

    pub use hidden::*;
    pub use source::*;
}

pub mod crate_visible_collision {
    mod source {
        pub struct CrateCandidate;
    }
    mod competing {
        pub(crate) use super::source::CrateCandidate as CrateExport;
    }
    mod exported {
        pub struct CrateExport;
    }
    mod inner {
        use super::competing::*;
        pub use super::exported::*;
    }

    pub use inner::*;
}

pub mod repeated_target {
    mod source {
        pub struct RepeatedCandidate;
    }
    mod exported {
        pub struct RepeatedExport;
    }
    mod hidden {
        use super::source::RepeatedCandidate as RepeatedExport;
        pub use inner::*;
        mod inner {
            pub use super::super::exported::*;
            pub use super::*;
        }
    }

    // Reaching `hidden` first from this scope must not suppress a later visit from `inner`,
    // where its private named import is accessible and conflicts with the public candidate.
    pub use hidden::*;
}

#[allow(hidden_glob_reexports)]
pub mod independent_public_route {
    mod exported {
        pub struct IndependentExport;
        pub use IndependentExport as Collision;
    }
    mod hidden {
        struct Collision;
        mod inner {
            pub use super::super::exported::*;
            pub use super::*;
        }
        pub use inner::*;
    }

    pub use exported::*;
    pub use hidden::*;
}
