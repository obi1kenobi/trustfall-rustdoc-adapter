#![no_std]
#![feature(prelude_import)]
#![expect(
    internal_features,
    reason = "Exercise rustdoc output for compiler prelude imports."
)]

mod local_prelude {
    pub struct PreludeOnly;
}

// Keep this public so treating the directive as an ordinary glob would reexport `PreludeOnly`.
#[prelude_import]
pub use local_prelude::*;

mod ordinary_exports {
    pub struct Exported;
}

pub use ordinary_exports::*;
