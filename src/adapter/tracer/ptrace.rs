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

struct PreActionIter<T, I: Iterator<Item = T>, F: Fn()> {
    inner: I,
    pre_action: F,
}

impl<T, I, F> Iterator for PreActionIter<T, I, F>
where
    F: Fn(),
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        (self.pre_action)();
        self.inner.next()
    }
}

struct OnIterEnd<T, I: Iterator<Item = T>, F: FnOnce()> {
    inner: I,
    on_end_func: Option<F>,
}

impl<T, I, F> Iterator for OnIterEnd<T, I, F>
where
    F: FnOnce(),
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.inner.next();
        if result.is_none() {
            let end_func = self.on_end_func.take();
            if let Some(func) = end_func {
                func();
            }
        }
        result
    }
}

fn make_iter_with_pre_action<T, I: Iterator<Item = T>, F: Fn()>(
    inner: I,
    pre_action: F,
) -> PreActionIter<T, I, F> {
    PreActionIter { inner, pre_action }
}

fn make_iter_with_end_action<T, I: Iterator<Item = T>, F: FnOnce()>(
    inner: I,
    on_end: F,
) -> OnIterEnd<T, I, F> {
    OnIterEnd {
        inner,
        on_end_func: Some(on_end),
    }
}

pub trait VertexT: Clone + Debug {}
impl<T: Clone + Debug> VertexT for T {}

/// The id of an operation.
// PERF: NonZeroU32 is used instead of NonZeroUsize to reduce the size
// of TraceOp by 8 bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct Opid(pub NonZeroU32);

/// Records and stores operations performed by the adapter.
///
/// This struct is intended for use inside of a TracingAdapter.
/// Operations must be recorded sequentially in chronological order.
/// Recording out-of-order operations will lead to invalid state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tracer {
    pub ops: Vec<TraceOp>,
}

impl Tracer {
    /// Construct a new tracer.
    pub fn new() -> Self {
        Self {
            ops: Vec::with_capacity(100_000),
        }
    }

    /// Record an operation.
    pub fn record(
        &mut self,
        content: TraceOpType,
        parent: Option<Opid>,
        duration: Option<Duration>,
    ) -> Opid {
        let size_u32 =
            u32::try_from(self.ops.len()).expect("operations should be smaller than u32::max") + 1;
        let next_opid = Opid(NonZeroU32::new(size_u32).unwrap());

        let op = TraceOp {
            opid: next_opid,
            parent_opid: parent,
            duration: duration,
            content,
        };
        self.ops.push(op);
        next_opid
    }

    pub fn operations(&self) -> &Vec<TraceOp> {
        &self.ops
    }
}

/// An operation performed by the adapter.
/// A parent_opid of None means that it is a top-level operation.
/// A TraceOp is only intended to be constructed by a Tracer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceOp {
    pub opid: Opid,                 // 4 bytes
    pub parent_opid: Option<Opid>,  // 4 bytes
    pub duration: Option<Duration>, // 16 bytes (12 + alignment)
    pub content: TraceOpType,       // 8 bytes
}

/// The type of an operation.
/// Each type corresponds to a particular operation performed by an adaper.
/// Call: A function is called.
/// AdvanceInputIterator: An iterator input to a function is incremented.
/// YieldInto: An input function returns a value. Called from the function
///     which takes input.
/// YieldFrom: A function returns a value. Called from inside the function.
/// InputIteratorExhausted: No further inputs available
/// OutputIteratorExhausted: No further outputs available
/// ProduceQueryResult: A result is returned from a query.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceOpType {
    // FunctionCall is boxed to reduce the size of TraceOpType.
    Call(Box<FunctionCall>),

    AdvanceInputIterator,
    YieldInto,
    YieldFrom(YieldValue),

    InputIteratorExhausted,
    OutputIteratorExhausted,

    ProduceQueryResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum YieldValue {
    ResolveStartingVertices,
    ResolveProperty,
    ResolveNeighborsOuter,
    ResolveNeighborsInner,
    ResolveCoercion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionCall {
    ResolveStartingVertices(Vid),             // vertex ID
    ResolveProperty(Vid, Arc<str>, Arc<str>), // vertex ID + type name + name of the property
    ResolveNeighbors(Vid, Arc<str>, Eid),     // vertex ID + type name + edge ID
    ResolveCoercion(Vid, Arc<str>, Arc<str>), // vertex ID + current type + coerced-to type
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
    AdapterT::Vertex: VertexT + 'vertex,
{
    pub tracer: Rc<RefCell<Tracer>>,
    inner: AdapterT,
    _phantom: PhantomData<&'vertex ()>,
}

impl<'vertex, AdapterT> TracingAdapter<'vertex, AdapterT>
where
    AdapterT: Adapter<'vertex>,
    AdapterT::Vertex: VertexT + 'vertex,
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

pub fn trace_results<'vertex, AdapterT>(
    adapter_tap: Arc<TracingAdapter<'vertex, AdapterT>>,
    result_iter: impl Iterator<Item = BTreeMap<Arc<str>, FieldValue>> + 'vertex,
) -> impl Iterator<Item = BTreeMap<Arc<str>, FieldValue>> + 'vertex
where
    AdapterT: Adapter<'vertex> + 'vertex,
    AdapterT::Vertex: VertexT + 'vertex,
{
    Box::new(make_iter_with_perf_span(result_iter, move |result, d| {
        adapter_tap
            .tracer
            .borrow_mut()
            .record(TraceOpType::ProduceQueryResult, None, Some(d));
        result
    }))
}

impl<'vertex, AdapterT> Adapter<'vertex> for TracingAdapter<'vertex, AdapterT>
where
    AdapterT: Adapter<'vertex> + 'vertex,
    AdapterT::Vertex: VertexT + 'vertex,
{
    type Vertex = AdapterT::Vertex;

    fn resolve_starting_vertices(
        &self,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        resolve_info: &ResolveInfo,
    ) -> VertexIterator<'vertex, Self::Vertex> {
        let mut trace = self.tracer.borrow_mut();
        let call_opid = trace.record(
            TraceOpType::Call(Box::new(FunctionCall::ResolveStartingVertices(
                resolve_info.vid(),
            ))),
            None,
            None,
        );
        drop(trace);

        let inner_iter = self
            .inner
            .resolve_starting_vertices(edge_name, parameters, resolve_info);
        let tracer_ref_1 = self.tracer.clone();
        let tracer_ref_2 = self.tracer.clone();
        let x = make_iter_with_perf_span(inner_iter, move |v, d| {
            tracer_ref_1.borrow_mut().record(
                TraceOpType::YieldFrom(YieldValue::ResolveStartingVertices),
                Some(call_opid),
                Some(d),
            );
            v
        });

        Box::new(make_iter_with_end_action(x, move || {
            tracer_ref_2.borrow_mut().record(
                TraceOpType::OutputIteratorExhausted,
                Some(call_opid),
                None,
            );
        }))
    }

    fn resolve_property<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        property_name: &Arc<str>,
        resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'vertex, V, FieldValue> {
        let mut trace = self.tracer.borrow_mut();
        let call_opid = trace.record(
            TraceOpType::Call(Box::new(FunctionCall::ResolveProperty(
                resolve_info.vid(),
                type_name.clone(),
                property_name.clone(),
            ))),
            None,
            None,
        );
        drop(trace);

        let tracer_ref_1 = self.tracer.clone();
        let tracer_ref_2 = self.tracer.clone();
        let tracer_ref_3 = self.tracer.clone();

        let x = make_iter_with_perf_span(contexts, move |context, d| {
            tracer_ref_3
                .borrow_mut()
                .record(TraceOpType::YieldInto, Some(call_opid), Some(d));
            context
        });

        let wrapped_contexts = Box::new(make_iter_with_end_action(
            make_iter_with_pre_action(x, move || {
                tracer_ref_1.borrow_mut().record(
                    TraceOpType::AdvanceInputIterator,
                    Some(call_opid),
                    None,
                );
            }),
            move || {
                tracer_ref_2.borrow_mut().record(
                    TraceOpType::InputIteratorExhausted,
                    Some(call_opid),
                    None,
                );
            },
        ));

        let inner_iter =
            self.inner
                .resolve_property(wrapped_contexts, type_name, property_name, resolve_info);

        let tracer_ref_4 = self.tracer.clone();
        let tracer_ref_5 = self.tracer.clone();

        let x = make_iter_with_perf_span(inner_iter, move |(context, value), d| {
            tracer_ref_5.borrow_mut().record(
                TraceOpType::YieldFrom(YieldValue::ResolveProperty),
                Some(call_opid),
                Some(d),
            );
            (context, value)
        });

        Box::new(make_iter_with_end_action(x, move || {
            tracer_ref_4.borrow_mut().record(
                TraceOpType::OutputIteratorExhausted,
                Some(call_opid),
                None,
            );
        }))
    }

    fn resolve_neighbors<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        edge_name: &Arc<str>,
        parameters: &EdgeParameters,
        resolve_info: &ResolveEdgeInfo,
    ) -> ContextOutcomeIterator<'vertex, V, VertexIterator<'vertex, Self::Vertex>> {
        let mut trace = self.tracer.borrow_mut();
        let call_opid = trace.record(
            TraceOpType::Call(Box::new(FunctionCall::ResolveNeighbors(
                resolve_info.origin_vid(),
                type_name.clone(),
                resolve_info.eid(),
            ))),
            None,
            None,
        );
        drop(trace);

        let tracer_ref_1 = self.tracer.clone();
        let tracer_ref_2 = self.tracer.clone();
        let tracer_ref_3 = self.tracer.clone();

        let x = make_iter_with_perf_span(contexts, move |context, d| {
            tracer_ref_3
                .borrow_mut()
                .record(TraceOpType::YieldInto, Some(call_opid), Some(d));
            context
        });

        let wrapped_contexts = Box::new(make_iter_with_end_action(
            make_iter_with_pre_action(x, move || {
                tracer_ref_1.borrow_mut().record(
                    TraceOpType::AdvanceInputIterator,
                    Some(call_opid),
                    None,
                );
            }),
            move || {
                tracer_ref_2.borrow_mut().record(
                    TraceOpType::InputIteratorExhausted,
                    Some(call_opid),
                    None,
                );
            },
        ));

        let inner_iter = self.inner.resolve_neighbors(
            wrapped_contexts,
            type_name,
            edge_name,
            parameters,
            resolve_info,
        );

        let tracer_ref_4 = self.tracer.clone();
        let tracer_ref_5 = self.tracer.clone();

        let x = make_iter_with_perf_span(inner_iter, move |(context, neighbor_iter), d| {
            let mut trace = tracer_ref_5.borrow_mut();
            let outer_iterator_opid = trace.record(
                TraceOpType::YieldFrom(YieldValue::ResolveNeighborsOuter),
                Some(call_opid),
                Some(d),
            );
            drop(trace);

            let tracer_ref_6 = tracer_ref_5.clone();
            let tapped_neighbor_iter = Box::new(
                make_iter_with_perf_span(neighbor_iter.enumerate(), move |(pos, vertex), d| {
                    tracer_ref_6.borrow_mut().record(
                        TraceOpType::YieldFrom(YieldValue::ResolveNeighborsInner),
                        Some(outer_iterator_opid),
                        Some(d),
                    );
                    (pos, vertex)
                })
                .map(move |(_, vertex)| vertex),
            );

            let tracer_ref_7 = tracer_ref_5.clone();
            let final_neighbor_iter: VertexIterator<'vertex, Self::Vertex> =
                Box::new(make_iter_with_end_action(tapped_neighbor_iter, move || {
                    tracer_ref_7.borrow_mut().record(
                        TraceOpType::OutputIteratorExhausted,
                        Some(outer_iterator_opid),
                        None,
                    );
                }));

            (context, final_neighbor_iter)
        });

        Box::new(make_iter_with_end_action(x, move || {
            tracer_ref_4.borrow_mut().record(
                TraceOpType::OutputIteratorExhausted,
                Some(call_opid),
                None,
            );
        }))
    }

    fn resolve_coercion<V: AsVertex<Self::Vertex> + 'vertex>(
        &self,
        contexts: ContextIterator<'vertex, V>,
        type_name: &Arc<str>,
        coerce_to_type: &Arc<str>,
        resolve_info: &ResolveInfo,
    ) -> ContextOutcomeIterator<'vertex, V, bool> {
        let mut trace = self.tracer.borrow_mut();
        let call_opid = trace.record(
            TraceOpType::Call(Box::new(FunctionCall::ResolveCoercion(
                resolve_info.vid(),
                type_name.clone(),
                coerce_to_type.clone(),
            ))),
            None,
            None,
        );
        drop(trace);

        let tracer_ref_1 = self.tracer.clone();
        let tracer_ref_2 = self.tracer.clone();
        let tracer_ref_3 = self.tracer.clone();

        let x = Box::new(make_iter_with_perf_span(contexts, move |context, d| {
            tracer_ref_3
                .borrow_mut()
                .record(TraceOpType::YieldInto, Some(call_opid), Some(d));
            context
        }));

        let wrapped_contexts = Box::new(make_iter_with_end_action(
            make_iter_with_pre_action(x, move || {
                tracer_ref_1.borrow_mut().record(
                    TraceOpType::AdvanceInputIterator,
                    Some(call_opid),
                    None,
                );
            }),
            move || {
                tracer_ref_2.borrow_mut().record(
                    TraceOpType::InputIteratorExhausted,
                    Some(call_opid),
                    None,
                );
            },
        ));

        let inner_iter =
            self.inner
                .resolve_coercion(wrapped_contexts, type_name, coerce_to_type, resolve_info);

        let tracer_ref_4 = self.tracer.clone();
        let tracer_ref_5 = self.tracer.clone();

        let x = Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, can_coerce), d| {
                tracer_ref_5.borrow_mut().record(
                    TraceOpType::YieldFrom(YieldValue::ResolveCoercion),
                    Some(call_opid),
                    Some(d),
                );
                (context, can_coerce)
            },
        ));

        Box::new(make_iter_with_end_action(x, move || {
            tracer_ref_4.borrow_mut().record(
                TraceOpType::OutputIteratorExhausted,
                Some(call_opid),
                None,
            );
        }))
    }
}
