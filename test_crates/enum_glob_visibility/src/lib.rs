#![allow(dead_code, unused_imports)]
#![deny(ambiguous_glob_imports)]

pub mod private_enum {
    enum PrivateEnum {
        PrivateVariant,
    }

    pub use PrivateEnum::*;
}

pub mod crate_enum {
    pub(crate) enum CrateEnum {
        CrateVariant,
    }

    pub use CrateEnum::*;
}

pub mod super_enum {
    mod source {
        pub(super) enum SuperEnum {
            SuperVariant,
        }

        pub use SuperEnum::*;
    }

    pub use source::*;
}

pub mod restricted_enum {
    mod outer {
        mod source {
            pub(in crate::restricted_enum) enum RestrictedEnum {
                RestrictedVariant,
            }

            pub use RestrictedEnum::*;
        }

        pub use source::*;
    }

    pub use outer::*;
}

pub mod nested_globs {
    enum NestedEnum {
        NestedVariant,
    }
    mod first {
        pub use super::NestedEnum::*;
    }
    mod second {
        pub use super::first::*;
    }

    pub use second::*;
}

pub mod public_enum {
    pub enum PublicEnum {
        PublicVariant,
    }

    pub use PublicEnum::*;
}

mod private_source {
    pub enum PublicInPrivateModule {
        PrivateModuleVariant,
    }
}

pub mod private_module {
    pub use super::private_source::PublicInPrivateModule::*;
}

pub mod private_alias {
    mod source {
        pub enum AliasTarget {
            AliasVariant,
        }
    }

    pub use PrivateAlias::*;
    use source::AliasTarget as PrivateAlias;
}

pub mod collision {
    enum CollisionEnum {
        Collision,
    }
    mod competing {
        pub use super::CollisionEnum::*;
    }
    mod exported {
        #[allow(non_upper_case_globals)]
        pub const Collision: () = ();
    }
    mod output {
        use super::competing::*;
        pub use super::exported::*;
    }

    pub use output::*;
}
