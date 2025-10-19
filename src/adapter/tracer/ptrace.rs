use std::{
    cell::RefCell, collections::BTreeMap, fmt::Debug, marker::PhantomData, rc::Rc, sync::Arc,
    time::Duration,
};

use std::time::Instant;

use trustfall::{
    FieldValue,
    provider::{
        Adapter, AsVertex, ContextIterator, ContextOutcomeIterator, EdgeParameters, Eid,
        ResolveEdgeInfo, ResolveInfo, VertexInfo, VertexIterator, Vid,
    },
};

/// A simple histogram that stores 15 roughly exponentially increasing buckets of
/// values, from 0 to 1 billion, followed by a final bucket to store numbers greater
/// than 1 billion.
#[derive(Clone)]
pub struct ExpHistogram {
    buckets: [u32; 16],
}

/// The largest value that will be accepted into each bucket of the histogram.
pub const HIST_BOUNDARIES: [u64; 16] = [
    100,
    300,
    1000,
    3000,
    10000,
    30000,
    100000,
    300000,
    1000000,
    3000000,
    10000000,
    30000000,
    100000000,
    300000000,
    1000000000,
    u64::MAX,
];

impl ExpHistogram {
    /// Create a new histogram.
    pub fn new() -> ExpHistogram {
        ExpHistogram { buckets: [0; 16] }
    }

    /// Add a value to the histogram.
    pub fn add(&mut self, num: u64) {
        for (i, lim) in HIST_BOUNDARIES.iter().enumerate() {
            if num <= *lim {
                self.buckets[i] = self.buckets[i].saturating_add(1);
                break;
            }
        }
    }

    /// Returns the largest value that will be accepted into each bucket.
    pub fn boundaries(&self) -> &'static [u64; 16] {
        &HIST_BOUNDARIES
    }

    /// Returns the number of values stored in the histogram.
    pub fn count(&self) -> u32 {
        self.buckets()
            .iter()
            .copied()
            .fold(0, |acc, num| acc.saturating_add(num))
    }

    /// Returns the count of each bucket
    pub fn buckets(&self) -> &[u32; 16] {
        &self.buckets
    }
}

impl Debug for ExpHistogram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExpHistogram {{ buckets: {:?}, count: {:?} }}",
            &self.buckets,
            self.count()
        )
    }
}

impl Default for ExpHistogram {
    fn default() -> Self {
        Self::new()
    }
}

/// A summary of key timing statistics.
#[derive(Debug, Clone)]
pub struct Summary {
    hist: ExpHistogram,
    min: Duration,
    max: Duration,
    sum: Duration,
}

impl Summary {
    // By initialising with a duration, we don't require min/max to be options.
    pub fn new(duration: Duration) -> Summary {
        let mut hist = ExpHistogram::new();
        hist.add(duration.as_nanos() as u64);

        Summary {
            hist,
            min: duration,
            max: duration,
            sum: duration,
        }
    }

    /// Add a new time to the summary.
    pub fn update(&mut self, duration: Duration) {
        self.hist.add(duration.as_nanos() as u64);

        self.min = self.min.min(duration);
        self.max = self.max.max(duration);
        self.sum = self.sum.saturating_add(duration);
    }

    /// Returns the number of items that have been processed.
    pub fn count(&self) -> u32 {
        self.hist.count()
    }

    /// Returns the total time
    pub fn total(&self) -> Duration {
        self.sum
    }

    /// Returns the fastest operation
    pub fn min(&self) -> Duration {
        self.min
    }

    /// Returns the slowest operation
    pub fn max(&self) -> Duration {
        self.max
    }

    /// Returns the exponential histogram with times in nanoseconds.
    pub fn histogram(&self) -> &ExpHistogram {
        &self.hist
    }

    /// Returns the mean duration.
    pub fn mean(&self) -> Duration {
        // This cannot panic since `count()` is always at least 1.
        self.sum / self.count()
    }
}

/// Records and stores operations performed by the adapter.
///
/// This struct is intended for use inside of a [`TracingAdapter`].
/// Operations must be recorded sequentially in chronological order.
/// Recording out-of-order operations will lead to invalid state.
#[derive(Debug, Clone)]
pub struct Tracer {
    pub calls: BTreeMap<FunctionCall, Summary>,

    // When we measure the time of an iterator, we also measure the time spent
    // evaluating its inputs. We must therefore subtract out this time when
    // we record the time each operation takes.
    last_input_duration: Option<Duration>,
}

impl Tracer {
    /// Construct a new tracer.
    pub fn new() -> Self {
        Self {
            calls: BTreeMap::new(),
            last_input_duration: None,
        }
    }

    /// Record an operation.
    pub fn record_time(&mut self, call_id: &FunctionCall, duration: Duration) {
        if let Some(summary) = self.calls.get_mut(call_id) {
            summary.update(duration);
        } else {
            self.calls.insert(call_id.clone(), Summary::new(duration));
        }
    }

    /// Set the duration of the last input.
    pub fn record_last_input_duration(&mut self, duration: Duration) {
        self.last_input_duration = Some(duration);
    }

    /// Get the duration of the last input. Panics if the duration is `None`.
    pub fn get_last_input_duration(&self) -> Duration {
        self.last_input_duration.unwrap()
    }
}

impl Default for Tracer {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionCall {
    ResolveProperty(Vid, Arc<str>, Arc<str>), // vertex ID + type name + name of the property
    ResolveNeighbors(Vid, Arc<str>, Eid),     // vertex ID + type name + edge ID
    ResolveNeighborsInner(Vid, Arc<str>, Eid), // same as ResolveNeighbors
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

fn make_iter_with_perf_span<I, T, F>(inner: I, post_action: F) -> PerfSpanIter<I, T, F>
where
    I: Iterator<Item = T>,
    F: Fn(T, Duration) -> T,
{
    PerfSpanIter { inner, post_action }
}

/// Traces an inner adapter. Constructed with [`TracingAdapter::new()`]. Each
/// tracer is only valid for one query. When switching between queries, if you
/// use the same adapter, ensure you first call [`TracingAdapter::finish()`],
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
    pub fn new(adapter: AdapterT) -> Self {
        Self {
            tracer: Rc::new(RefCell::new(Tracer::new())),
            inner: adapter,
            _phantom: PhantomData,
        }
    }

    /// Finalise the trace and return it.
    pub fn finish(&self) -> Tracer {
        // Ensure nothing is reading the trace i.e. we can safely stop interpreting.
        let trace_ref = self.tracer.borrow_mut();
        let new_trace = Tracer::new();
        drop(trace_ref);
        self.tracer.replace(new_trace)
    }
}

// For all resolutions we want to know:
// 1. What are we resolving?
// 2. How long did the resolution take?
// 3. How many times was a specific resolution resolved?
//     e.g. resolve_property(Vid(1), "Trait", "name"))
//
// We are not collecting:
// (1) Number of times a function is called, and time spent constructing
//     the iterators.
//
//     The number of times any given resolution function is called is
//     an implementation detail of trustfall, and since most time is spent
//     resolving iterators, tracking it isn't valuable.
//
// (2) Whether or not the returned iterator is empty.
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
        let call_id = FunctionCall::ResolveProperty(
            resolve_info.vid(),
            type_name.clone(),
            property_name.clone(),
        );

        let tracer_ref = self.tracer.clone();

        let wrapped_contexts = Box::new(make_iter_with_perf_span(
            contexts,
            move |context, duration| {
                tracer_ref.borrow_mut().record_last_input_duration(duration);
                context
            },
        ));

        let inner_iter =
            self.inner
                .resolve_property(wrapped_contexts, type_name, property_name, resolve_info);

        let tracer_ref_2 = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, value), duration| {
                let input_duration = tracer_ref_2.borrow().get_last_input_duration();
                tracer_ref_2
                    .borrow_mut()
                    .record_time(&call_id, duration - input_duration);
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
        // Inner and outer call times are often quite different, so they need to
        // be tracked separately.
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

        let tracer_ref = self.tracer.clone();

        let wrapped_contexts = Box::new(make_iter_with_perf_span(
            contexts,
            move |context, duration| {
                tracer_ref.borrow_mut().record_last_input_duration(duration);
                context
            },
        ));

        let inner_iter = self.inner.resolve_neighbors(
            wrapped_contexts,
            type_name,
            edge_name,
            parameters,
            resolve_info,
        );

        let tracer_ref_2 = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, neighbor_iter), duration| {
                let input_duration = tracer_ref_2.borrow().get_last_input_duration();
                tracer_ref_2
                    .borrow_mut()
                    .record_time(&call_id, duration - input_duration);

                let tracer_ref_3 = tracer_ref_2.clone();

                let value = call_id_inner.clone();

                // We do not subtract the input duration for the inner iterator
                // because there is no input.
                let tapped_neighbor_iter = Box::new(make_iter_with_perf_span(
                    neighbor_iter,
                    move |vertex, duration| {
                        tracer_ref_3.borrow_mut().record_time(&value, duration);
                        vertex
                    },
                ));

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

        let tracer_ref = self.tracer.clone();

        let wrapped_contexts = Box::new(make_iter_with_perf_span(
            contexts,
            move |context, duration| {
                tracer_ref.borrow_mut().record_last_input_duration(duration);
                context
            },
        ));

        let inner_iter =
            self.inner
                .resolve_coercion(wrapped_contexts, type_name, coerce_to_type, resolve_info);

        let tracer_ref_2 = self.tracer.clone();

        Box::new(make_iter_with_perf_span(
            inner_iter,
            move |(context, can_coerce), duration| {
                let input_duration = tracer_ref_2.borrow().get_last_input_duration();
                tracer_ref_2
                    .borrow_mut()
                    .record_time(&call_id, duration - input_duration);
                (context, can_coerce)
            },
        ))
    }
}
