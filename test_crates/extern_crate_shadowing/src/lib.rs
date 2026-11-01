#![allow(dead_code, unused_imports)]
#![deny(ambiguous_glob_imports)]

pub struct Marker;

mod items {
    pub struct SelfShadowed {}
    pub const SELF_VALUE: u8 = 1;
    pub struct CoreShadowed {}
    pub const CORE_VALUE: u8 = 2;
    pub struct UnrenamedShadowed {}
    pub const UNRENAMED_VALUE: u8 = 3;
    pub struct NestedShadowed {}
    pub const NESTED_VALUE: u8 = 4;
    pub struct OriginalCoreName {}
    pub struct AnonymousCoreName {}
}

pub use items::SelfShadowed as IndependentType;

mod self_source {
    pub use crate::items::SELF_VALUE as SelfAlias;
    pub use crate::items::SelfShadowed as SelfAlias;
}

pub mod self_alias {
    pub extern crate self as SelfAlias;
    pub use crate::self_source::*;
}

mod core_source {
    pub use crate::items::CORE_VALUE as ExternalAlias;
    pub use crate::items::CoreShadowed as ExternalAlias;
    pub use crate::items::OriginalCoreName as core;
}

pub mod core_alias {
    pub extern crate core as ExternalAlias;
    pub use crate::core_source::*;
}

mod unrenamed_source {
    pub use crate::items::UNRENAMED_VALUE as core;
    pub use crate::items::UnrenamedShadowed as core;
}

pub mod unrenamed {
    pub extern crate core;
    pub use crate::unrenamed_source::*;
}

mod nested_source {
    pub use crate::items::NESTED_VALUE as NestedAlias;
    pub use crate::items::NestedShadowed as NestedAlias;
}
mod nested_scope {
    pub extern crate self as NestedAlias;
    pub use crate::nested_source::*;
}

pub mod nested {
    pub use crate::nested_scope::*;
}

mod anonymous_source {
    pub use crate::items::AnonymousCoreName as core;
}

pub mod anonymous {
    pub extern crate core as _;
    pub use crate::anonymous_source::*;
}
