#![allow(dead_code, unused_imports)]
#![deny(ambiguous_glob_imports)]

mod values {
    pub const PRIVATE_FIELD_VALUE: u8 = 0;
    pub const CRATE_FIELD_VALUE: u8 = 1;
    pub const PRIVATE_STRUCT_VALUE: u8 = 2;
    pub const CRATE_STRUCT_VALUE: u8 = 3;
    pub const BRACE_STRUCT_VALUE: u8 = 4;
    pub const NAMED_IMPORT_VALUE: u8 = 5;
    pub const GLOB_IMPORT_VALUE: u8 = 6;
    pub const NESTED_VALUE: u8 = 7;
}

pub use values::PRIVATE_FIELD_VALUE;

pub mod private_field {
    pub struct PrivateField(bool);

    mod renamed {
        pub use crate::values::PRIVATE_FIELD_VALUE as PrivateField;
    }

    pub use renamed::*;
}

pub mod crate_field {
    pub struct CrateField(pub(crate) bool);

    mod renamed {
        pub use crate::values::CRATE_FIELD_VALUE as CrateField;
    }

    pub use renamed::*;
}

pub mod private_struct {
    struct PrivateStruct(bool);

    mod renamed {
        pub use crate::values::PRIVATE_STRUCT_VALUE as PrivateStruct;
    }

    pub use renamed::*;
}

pub mod crate_struct {
    pub(crate) struct CrateStruct(bool);

    mod renamed {
        pub use crate::values::CRATE_STRUCT_VALUE as CrateStruct;
    }

    pub use renamed::*;
}

mod constructors {
    pub struct PublicFields(pub bool);
    pub struct ZeroFields();
}

pub mod public_constructors {
    // These definitions shadow the imported types, leaving only the constructors.
    pub struct PublicFields {}
    pub struct ZeroFields {}

    pub use crate::constructors::*;
}

pub mod brace_struct {
    pub struct BraceStruct {
        field: bool,
    }

    mod renamed {
        pub use crate::values::BRACE_STRUCT_VALUE as BraceStruct;
    }

    pub use renamed::*;
}

mod imported_types {
    pub struct NamedImported(bool);
}

pub mod named_import {
    pub use crate::imported_types::NamedImported;

    mod renamed {
        pub use crate::values::NAMED_IMPORT_VALUE as NamedImported;
    }

    pub use renamed::*;
}

pub mod glob_import {
    mod source {
        pub struct GlobImported(bool);
    }
    mod renamed {
        pub use crate::values::GLOB_IMPORT_VALUE as GlobImported;
    }

    pub use renamed::*;
    pub use source::*;
}

mod nested_source {
    pub struct NestedField(bool);

    mod renamed {
        pub use crate::values::NESTED_VALUE as NestedField;
    }

    pub use renamed::*;
}

pub mod nested_globs {
    mod intermediate {
        pub use crate::nested_source::*;
    }

    pub use intermediate::*;
}
