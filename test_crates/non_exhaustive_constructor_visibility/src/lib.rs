#![allow(dead_code, unused_imports)]
#![deny(ambiguous_glob_imports)]

mod source {
    #[non_exhaustive]
    pub struct HiddenUnit;
    #[non_exhaustive]
    pub struct HiddenTuple(pub bool);
    #[non_exhaustive]
    pub struct HiddenZero();

    pub struct OrdinaryUnit;
    pub struct OrdinaryTuple(pub bool);

    #[non_exhaustive]
    pub struct Nested(pub bool);
    #[non_exhaustive]
    pub struct ExportedUnit;
    #[non_exhaustive]
    pub struct ExportedTuple(pub bool);
    #[non_exhaustive]
    pub struct ValueShadowed(pub bool);
    #[non_exhaustive]
    pub struct LocalUnit;
}

mod renamed {
    pub use crate::source::HiddenTuple as TupleName;
    pub use crate::source::HiddenUnit as UnitName;
    pub use crate::source::HiddenZero as ZeroName;
    pub use crate::source::OrdinaryTuple as OrdinaryTupleName;
    pub use crate::source::OrdinaryUnit as OrdinaryUnitName;
}

pub mod shadowed_types {
    // Only the constructor bindings survive this glob's type-name shadowing.
    pub struct UnitName {}
    pub struct TupleName {}
    pub struct ZeroName {}
    pub struct OrdinaryUnitName {}
    pub struct OrdinaryTupleName {}

    pub use crate::renamed::*;
}

pub use source::Nested as PublicNested;

mod nested_source {
    pub use crate::source::Nested as NestedName;
}
mod nested_relay {
    pub use crate::nested_source::*;
}

pub mod nested {
    pub struct NestedName {}
    pub use crate::nested_relay::*;
}

mod exported_types {
    pub use crate::source::{ExportedTuple, ExportedUnit};
}

pub mod type_exports {
    pub use crate::exported_types::*;
}

mod value_shadow_source {
    pub use crate::source::ValueShadowed as VALUE_SHADOW;
}

pub mod value_shadow {
    pub const VALUE_SHADOW: u8 = 0;
    pub use crate::value_shadow_source::*;
}

mod values {
    pub const SHADOWED_VALUE: u8 = 1;
}

pub mod local_shadow {
    pub use crate::source::LocalUnit as LocalConstructor;

    mod renamed_value {
        pub use crate::values::SHADOWED_VALUE as LocalConstructor;
    }
    pub use renamed_value::*;

    fn construct() {
        let _: LocalConstructor = LocalConstructor;
    }
}

#[non_exhaustive]
pub enum ExtensibleEnum {
    OrdinaryVariant,
}
pub use ExtensibleEnum::*;

pub enum Variants {
    #[non_exhaustive]
    ExtensibleVariant(u8),
}
pub use Variants::*;
