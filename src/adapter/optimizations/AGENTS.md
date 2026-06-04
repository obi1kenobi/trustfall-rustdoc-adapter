# Adapter Optimizations

Optimizations in this directory often use constraints from a mandatory child edge to speed up
resolution of a parent edge. Make sure to preserve the semantics of the edge being resolved.

When using `CandidateValue::Multiple` on a mandatory child edge to speed up the resolution
of a parent edge, treat the candidate values as a set union over indexed lookup results.
Keep in mind that multiple candidate values can often be reachable through the same intermediate
vertex; it's a bug to emit that intermediate vertex more than once, as that will cause
erroneous duplication of the query results. The intermediate vertex must only be emitted once,
not once per child vertex reachable through it.

Examples:
- `Crate.item` optimized through `importable_path.path`: one item may have several matching
  importable paths, so union by item ID. A bug here was fixed
  in https://github.com/obi1kenobi/trustfall-rustdoc-adapter/pull/1066
- `ImplOwner.impl` optimized through child `method.name`: one impl may contain several matching
  methods, so union by impl ID. A bug here was fixed
  in https://github.com/obi1kenobi/trustfall-rustdoc-adapter/pull/1064

When adding or changing an optimization that uses a child-edge constraint, always add
a regression test where one destination vertex satisfies multiple candidate values
whenever such a case is possible.
