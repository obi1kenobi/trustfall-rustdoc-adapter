cfg_if::cfg_if! {
    if #[cfg(feature = "rayon")] {
        pub(crate) use clashmap::ClashMap as HashMap;
        pub(crate) use clashmap::ReadOnlyView as ReadOnlyHashMap;
        pub(crate) use clashmap::ClashSet as HashSet;
        pub(crate) use clashmap::Entry as Entry;
        pub(crate) use clashmap::OccupiedEntry as OccupiedEntry;
        pub(crate) use clashmap::VacantEntry as VacantEntry;

        #[inline]
        pub(crate) fn to_read_only<K, V>(value: HashMap<K, V>) -> ReadOnlyHashMap<K, V>
        where
            K: Eq + std::hash::Hash,
        {
            value.into_read_only()
        }
    } else if #[cfg(feature = "rustc-hash")] {
        pub(crate) use rustc_hash::HashMap as HashMap;
        pub(crate) use rustc_hash::HashMap as ReadOnlyHashMap;
        pub(crate) use rustc_hash::HashSet as HashSet;
        pub(crate) use rustc_hash::Entry as Entry;
        pub(crate) use rustc_hash::OccupiedEntry as OccupiedEntry;
        pub(crate) use rustc_hash::VacantEntry as VacantEntry;

        #[inline]
        pub(crate) fn to_read_only<K, V>(value: HashMap<K, V>) -> ReadOnlyHashMap<K, V>
        where
            K: Eq + std::hash::Hash,
        {
            value
        }
    } else {
        pub(crate) use std::collections::HashMap as HashMap;
        pub(crate) use std::collections::HashMap as ReadOnlyHashMap;
        pub(crate) use std::collections::HashSet as HashSet;
        pub(crate) use std::collections::Entry as Entry;
        pub(crate) use std::collections::OccupiedEntry as OccupiedEntry;
        pub(crate) use std::collections::VacantEntry as VacantEntry;

        #[inline]
        pub(crate) fn to_read_only<K, V>(value: HashMap<K, V>) -> ReadOnlyHashMap<K, V>
        where
            K: Eq + std::hash::Hash,
        {
            value
        }
    }
}
