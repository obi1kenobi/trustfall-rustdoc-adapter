use std::default::Default;
use std::hash::{BuildHasher, Hash};

#[cfg(not(feature = "rustc-hash"))]
pub(crate) use std::collections::{HashMap, HashSet};

#[cfg(feature = "rustc-hash")]
pub(crate) use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

#[cfg(feature = "rustc-hash")]
pub(crate) type IndexMap<K, V> = indexmap::map::IndexMap<K, V, rustc_hash::FxBuildHasher>;

#[cfg(not(feature = "rustc-hash"))]
pub(crate) use indexmap::map::IndexMap;

/// Allow using new() and with_capacity() regardless of the hash algorithm.
/// See <https://github.com/tkaitchuck/aHash/issues/103> for more information.
#[allow(dead_code, reason = "used when the `rustc-hash` feature is enabled")]
pub(crate) trait HashMapExt {
    fn new() -> Self;
    fn with_capacity(x: usize) -> Self;
}

impl<K, V, S> HashMapExt for std::collections::HashMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher + Default,
{
    fn new() -> Self {
        std::collections::HashMap::with_hasher(S::default())
    }

    fn with_capacity(capacity: usize) -> Self {
        std::collections::HashMap::with_capacity_and_hasher(capacity, S::default())
    }
}

impl<K, V, S> HashMapExt for indexmap::map::IndexMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher + Default,
{
    fn new() -> Self {
        indexmap::map::IndexMap::with_hasher(S::default())
    }

    fn with_capacity(capacity: usize) -> Self {
        indexmap::map::IndexMap::with_capacity_and_hasher(capacity, S::default())
    }
}
