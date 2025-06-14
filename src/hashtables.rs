#[cfg(not(feature = "rustc-hash"))]
pub(crate) use std::collections::{HashMap, HashSet};

#[cfg(feature = "rustc-hash")]
pub(crate) use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
