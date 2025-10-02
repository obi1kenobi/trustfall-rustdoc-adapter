use std::{
    cell::RefCell, collections::BTreeMap, fmt::Debug, marker::PhantomData, rc::Rc, sync::Arc,
    time::Duration,
};

use kll_rs::KllDoubleSketch;

use std::time::Instant;

use trustfall::{
    FieldValue,
    provider::{
        Adapter, AsVertex, ContextIterator, ContextOutcomeIterator, EdgeParameters, Eid,
        ResolveEdgeInfo, ResolveInfo, VertexInfo, VertexIterator, Vid,
    },
};

#[derive(Debug)]
pub struct Summary {
    sketch: KllDoubleSketch,
    min: Duration,
    max: Duration,
    sum: Duration,
}

impl Summary {
    // By initialising with a duration, we don't require min/max to be options.
    pub fn new(duration: Duration) -> Summary {
        // Based on https://datasketches.apache.org/docs/KLL/KLLAccuracyAndSize.html
        // a K of 200 should give ~1.33% error.
        let mut sketch = KllDoubleSketch::new_with_k(200).unwrap();
        sketch.update(duration.as_nanos() as f64);

        Summary {
            sketch,
            min: duration,
            max: duration,
            sum: duration,
        }
    }

    /// Add a new time to the summary.
    pub fn update(&mut self, duration: Duration) {
        self.sketch.update(duration.as_nanos() as f64);

        self.min = self.min.min(duration);
        self.max = self.max.max(duration);
        self.sum += duration;
    }

    /// Returns the number of items that have been processed.
    pub fn count(&self) -> u64 {
        self.sketch.get_n()
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

    /// Returns the quantile (0 < quant < 1) time.
    pub fn quantile(&self, quant: f64) -> f64 {
        assert!(0.0 < quant && quant < 1.0);
        self.sketch.get_quantile(quant)
    }
}

/// Records and stores operations performed by the adapter.
///
/// This struct is intended for use inside of a TracingAdapter.
/// Operations must be recorded sequentially in chronological order.
/// Recording out-of-order operations will lead to invalid state.
#[derive(Debug)]
pub struct Tracer {
    calls: BTreeMap<FunctionCall, Summary>,

    // When we measure the time of an iterator, we also measure the time spent
    // evaluating its inputs. We must therefore subtract this time out when
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

    /// Get the duration of the last input. Panics if the duration is None.
    pub fn get_last_input_duration(&self) -> Duration {
        self.last_input_duration.unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[allow(clippy::enum_variant_names)] // Names match the adapter functions 
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
        // be stored separately.
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
