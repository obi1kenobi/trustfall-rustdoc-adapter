//! Query optimizations must preserve the semantics of the edge being resolved.
//!
//! Several optimizations in this module use constraints on a mandatory child edge to speed up
//! resolution of a parent edge. If multiple child-constraint values can match the same parent-edge
//! result, the optimization must treat those matches as a set union and emit the parent-edge result
//! once. The `HashSet` filters in those paths are not defensive deduplication; they preserve the
//! edge's result identity. An example of a duplication bug and fix involving the above
//! can be found here: <https://github.com/obi1kenobi/trustfall-rustdoc-adapter/pull/1066>.

pub(super) mod impl_lookup;
pub(super) mod item_lookup;
pub(super) mod method_lookup;
pub(super) mod variant_lookup;
