#[cfg(not(feature = "rustc-hash"))]
pub(crate) use std::collections::{HashMap, HashSet};

#[cfg(feature = "rustc-hash")]
pub(crate) use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

#[cfg(feature = "rustc-hash")]
pub(crate) type IndexMap<K, V> = indexmap::map::IndexMap<K, V, rustc_hash::FxBuildHasher>;

#[cfg(not(feature = "rustc-hash"))]
pub(crate) use indexmap::{map::IndexMap};
