mod hidden_glob_only_source {
    pub struct HiddenGlobOnly;
}

mod hidden_glob_only {
    #[doc(hidden)]
    pub use super::hidden_glob_only_source::*;
}

pub use hidden_glob_only::*;

mod hidden_and_visible_glob_source {
    pub struct HiddenAndVisibleGlob;
}

mod hidden_glob_path {
    #[doc(hidden)]
    pub use super::hidden_and_visible_glob_source::*;
}

mod visible_glob_path {
    pub use super::hidden_and_visible_glob_source::*;
}

pub use hidden_glob_path::*;
pub use visible_glob_path::*;

mod direct_use_source {
    pub struct HiddenGlobAndDirectUse;
}

mod hidden_glob_for_direct_use {
    #[doc(hidden)]
    pub use super::direct_use_source::*;
}

pub use hidden_glob_for_direct_use::*;
pub use direct_use_source::HiddenGlobAndDirectUse;
