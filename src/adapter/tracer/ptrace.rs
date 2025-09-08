use std::{
    cell::RefCell, collections::BTreeMap, fmt::Debug, marker::PhantomData, num::NonZeroU32, rc::Rc,
    sync::Arc, time::Duration,
};

use serde::{Deserialize, Serialize};

use std::time::Instant;

use trustfall::{
    FieldValue,
    provider::{
        Adapter, AsVertex, ContextIterator, ContextOutcomeIterator, EdgeParameters, Eid,
        ResolveEdgeInfo, ResolveInfo, VertexInfo, VertexIterator, Vid,
    },
};

/// Records and stores operations performed by the adapter.
///
/// This struct is intended for use inside of a TracingAdapter.
/// Operations must be recorded sequentially in chronological order.
/// Recording out-of-order operations will lead to invalid state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tracer {
    pub calls: BTreeMap<FunctionCall, Vec<Duration>>,
}

impl Tracer {
    /// Construct a new tracer.
    pub fn new() -> Self {
        Self {
            calls: BTreeMap::new(),
        }
    }

    /// Record an operation.
    pub fn record_time(&mut self, call_id: FunctionCall, duration: Duration) {
        self.calls
            .entry(call_id)
            .or_insert_with(|| Vec::with_capacity(1000))
            .push(duration);
    }

    pub fn calls(&self) -> &BTreeMap<FunctionCall, Vec<Duration>> {
        &self.calls
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionCall {
    ResolveStartingVertices(Vid),              // vertex ID
    ResolveProperty(Vid, Arc<str>, Arc<str>),  // vertex ID + type name + name of the property
    ResolveNeighbors(Vid, Arc<str>, Eid),      // vertex ID + type name + edge ID
    ResolveNeighborsInner(Vid, Arc<str>, Eid), // same as ResolveNeighbors
    ResolveCoercion(Vid, Arc<str>, Arc<str>),  // vertex ID + current type + coerced-to type
}

struct PerfSpanIter<I, T, F>
where
    I: Iterator<Item = T>,
    F: Fn(T, Duration) -> T,
{
    inner: I,
    post_action: F,
}

impl<I, T, F> Iterator for PerfSpanIter<I, T, F>
where
    I: Iterator<Item = T>,
    F: Fn(T, Duration) -> T,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let start = Instant::now();
        let item = self.inner.next();
        let time = start.elapsed();
        match item {
            Some(item) => Some((self.post_action)(item, time)),
            None => None,
        }
    }
}

fn make_iter_with_perf_span<I, T, F>(inner: I, post_action: F) -> PerfSpanIter<I, T, F>
where
    I: Iterator<Item = T>,
    F: Fn(T, Duration) -> T,
{
    PerfSpanIter { inner, post_action }
}

/// Traces an inner adapter. Constructed with new(). Each tracer is only valid
/// for one query. When switching between queries, ensure you first call finish(),
/// otherwise the new operations will be traced with the old.
#[derive(Debug, Clone)]
pub struct TracingAdapter<'vertex, AdapterT>
where
    AdapterT: Adapter<'vertex>,
    AdapterT::Vertex: Debug + Clone + 'vertex,
{
    pub tracer: Rc<RefCell<Tracer>>,
    inner: AdapterT,
    _phantom: PhantomData<&'vertex ()>,
}

impl<'vertex, AdapterT> TracingAdapter<'vertex, AdapterT>
where
    AdapterT: Adapter<'vertex>,
    AdapterT::Vertex: Debug + Clone + 'vertex,
{
    pub fn new(adapter: AdapterT, tracer: Rc<RefCell<Tracer>>) -> Self {
        Self {
            tracer,
            inner: adapter,
            _phantom: PhantomData,
        }
    }

    pub fn finish(self) -> Tracer {
        // Ensure nothing is reading the trace i.e. we can safely stop interpreting.
        let trace_ref = self.tracer.borrow_mut();
        let new_trace = Tracer::new();
        drop(trace_ref);
        self.tracer.replace(new_trace)
    }
}

impl<'vertex, AdapterT> Adapter<'vertex> for TracingAdapter<'vertex, AdapterT>
where
    AdapterT: Adapter<'vertex> + 'vertex,
    AdapterT::Vertex: Debug + Clone + 'vertex,
{
    type Vertex = AdapterT::Vertex;

    fn resolve_starting_vertices(
        &self,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        resolve_info: &ResolveInfo,
    ) -> VertexIterator<'vertex, Self::Vertex> {
        // We don't care about the first resolution since it's normally quick
        // and it only occurs once.
        self.inner
            .resolve_starting_vertices(edge_name, parameters, resolve_info)
    }

    fn resolve_property<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        property_name: &Arc<str>,
        resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'vertex, V, FieldValue> {
        // For each resolution we want to know:
        // 1. What are we resolving?
        // 2. How long did the resolution take?
        // 3. How many times did we resolve the same property?
        //
        // We are not collecting:
        // 1. Number of times a function is called.
        // 2. Whether or not any results were returned.

        let call_id = FunctionCall::ResolveProperty(
            resolve_info.vid(),
            type_name.clone(),
            property_name.clone(),
        );

        let inner_iter =
            self.inner
                .resolve_property(contexts, type_name, property_name, resolve_info);

        let tracer_ref = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, value), duration| {
                tracer_ref
                    .borrow_mut()
                    .record_time(call_id.clone(), duration);
                (context, value)
            },
        ))
    }

    fn resolve_neighbors<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        resolve_info: &ResolveEdgeInfo,
    ) -> ContextOutcomeIterator<'vertex, V, VertexIterator<'vertex, Self::Vertex>> {
        // Along with the standard information, we also want to know
        // how many times each inner iterator yielded.
        //
        // While in most cases the time spent in inner iterators will be
        // overshadowed by time spent in outer iterators, it can be significant.
        //
        // inner and outer call times are often quite different, so they need to
        // be stored differently.
        let call_id = FunctionCall::ResolveNeighbors(
            resolve_info.origin_vid(),
            type_name.clone(),
            resolve_info.eid(),
        );
        let call_id_inner = FunctionCall::ResolveNeighborsInner(
            resolve_info.origin_vid(),
            type_name.clone(),
            resolve_info.eid(),
        );

        let inner_iter =
            self.inner
                .resolve_neighbors(contexts, type_name, edge_name, parameters, resolve_info);

        let tracer_ref = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, neighbor_iter), duration| {
                tracer_ref
                    .borrow_mut()
                    .record_time(call_id.clone(), duration);

                let tracer_ref_2 = tracer_ref.clone();

                let value = call_id_inner.clone();

                let tapped_neighbor_iter = Box::new(
                    make_iter_with_perf_span(
                        neighbor_iter.enumerate(),
                        move |(pos, vertex), duration| {
                            tracer_ref_2
                                .borrow_mut()
                                .record_time(value.clone(), duration);
                            (pos, vertex)
                        },
                    )
                    .map(|(_, vertex)| vertex),
                );

                (context, tapped_neighbor_iter)
            },
        ))
    }

    fn resolve_coercion<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        coerce_to_type: &Arc<str>,
        resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'vertex, V, bool> {
        let call_id = FunctionCall::ResolveCoercion(
            resolve_info.vid(),
            type_name.clone(),
            coerce_to_type.clone(),
        );

        let inner_iter =
            self.inner
                .resolve_coercion(contexts, type_name, coerce_to_type, resolve_info);

        let tracer_ref = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, can_coerce), duration| {
                tracer_ref
                    .borrow_mut()
                    .record_time(call_id.clone(), duration);
                (context, can_coerce)
            },
        ))
    }
}
