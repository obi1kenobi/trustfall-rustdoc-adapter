pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn fn_returns_nothing() {}

pub struct PublicType<T>(pub T);

pub struct LifetimeConst<'a, const N: usize>(pub &'a [u8; N]);

pub fn concrete_types(value: u64) -> bool {
    value > 0
}

pub fn generic_identity<T>(value: T) -> T {
    value
}

pub fn lifetime_ref<'long>(value: &'long str) -> &'long str {
    value
}

pub fn const_array<const N: usize>(value: [u8; N]) -> [u8; N] {
    value
}

pub fn path_types(
    value: PublicType<u8>,
    values: Vec<PublicType<u8>>,
) -> Option<PublicType<u8>> {
    let _ = values;
    Some(value)
}

pub fn lifetime_const_path_args<'a, const N: usize>(value: LifetimeConst<'a, N>) {
    let _ = value;
}

pub fn composite_types<'long, T, const N: usize>(
    tuple: (&'long [T], *const T, fn(T) -> T, [u8; N]),
    raw: *mut T,
) -> (&'long [T], *mut T) {
    (tuple.0, raw)
}

pub fn function_pointer(
    callback: for<'callback> unsafe fn(&'callback u8) -> &'callback u8,
) -> for<'callback> unsafe fn(&'callback u8) -> &'callback u8 {
    callback
}

pub fn function_pointer_nested_generics<'outer, T, const N: usize>(
    callback: for<'callback> fn(&'outer T, &'callback [T; N]) -> &'callback T,
) -> for<'callback> fn(&'outer T, &'callback [T; N]) -> &'callback T {
    callback
}

pub fn dyn_trait_lifetime<'long>(
    value: Box<dyn Send + Sync + 'long>,
) -> Box<dyn Sync + Send + 'long> {
    value
}

pub fn dyn_fn_two_arg(value: Box<dyn Fn(u8, u16) -> u32>) {
    let _ = value;
}

pub fn dyn_fn_pointer_output(
    callback: Box<dyn Fn() -> *const dyn Send>,
    callback_with_lifetime: Box<dyn Fn() -> *const (dyn Send) + 'static>,
) {
    let _ = (callback, callback_with_lifetime);
}

pub fn impl_trait_param(value: impl Into<String>) -> String {
    value.into()
}

pub fn generic_and_impl_trait_params<T>(
    generic: T,
    first: impl Into<String>,
    second: impl Clone,
) -> (T, String) {
    let _ = second;
    (generic, first.into())
}

pub fn nested_impl_trait_params<T>(
    generic: T,
    borrowed: &impl Clone,
    values: Vec<impl Into<String>>,
    nested_tuple: (impl Default, T),
) -> T {
    let _ = (generic, borrowed, values);
    nested_tuple.1
}

pub fn nested_assoc_impl_trait_param(
    value: impl Iterator<Item = impl Into<String>>,
    other: impl Clone,
) -> usize {
    let _ = other;
    value.count()
}

pub fn impl_trait_numbering_baseline(first: impl Clone, second: impl Copy) {
    let _ = (first, second);
}

pub fn impl_trait_numbering_nested_first(
    first: impl Iterator<Item = impl Clone>,
    second: impl Copy,
) -> usize {
    let _ = second;
    first.count()
}

pub fn impl_trait_numbering_nested_second(
    first: impl Clone,
    second: impl Iterator<Item = impl Copy>,
) -> usize {
    let _ = first;
    second.count()
}

pub fn multiple_impl_traits_single_param(pair: (impl Clone, impl Copy), later: impl Default) {
    let _ = (pair, later);
}

pub fn maybe_sized_impl_trait(value: &impl ?Sized) {
    let _ = value;
}

pub fn impl_fn_bound(value: impl Fn(u8) -> u16 + Clone) {
    let _ = value;
}

pub fn impl_fn_lifetime_bound<'long>(value: impl Fn(u8) -> u16 + Clone + 'long) {
    let _ = value;
}

pub fn impl_fn_two_args_bound(value: impl Fn(u8, u16) -> u32 + Clone) {
    let _ = value;
}

pub fn raw_pointer_impl_trait(value: *const impl Clone, mutable: *mut impl Copy) {
    let _ = (value, mutable);
}

pub fn slice_and_array_impl_trait(value: &[impl Clone], array: [impl Copy; 3]) {
    let _ = (value, array);
}

pub fn unit_and_single_tuple(unit: (), single: (impl Clone,)) {
    let _ = (unit, single);
}

pub fn higher_ranked_fn_pointer(callback: for<'a> fn(&'a u8) -> &'a u8) {
    let _ = callback;
}

pub fn unsafe_c_variadic_pointer(callback: unsafe extern "C" fn(u8, ...) -> u8) {
    let _ = callback;
}

pub fn abi_variant_fn_pointers(
    c_unwind: extern "C-unwind" fn(u8) -> u8,
    system: extern "system" fn(u8) -> u8,
    system_unwind: extern "system-unwind" fn(u8) -> u8,
    win64: extern "win64" fn(u8) -> u8,
    sysv64: extern "sysv64" fn(u8) -> u8,
) {
    let _ = (c_unwind, system, system_unwind, win64, sysv64);
}

pub fn dyn_pointer_and_mut_ref<'a>(
    pointer: *const (dyn Send + Sync + 'a),
    borrowed: &'a mut (dyn Send + Sync),
) {
    let _ = (pointer, borrowed);
}

pub fn impl_trait_return() -> impl Iterator<Item = u8> {
    0u8..=1
}

pub fn borrowed_opaque_return() -> &'static (impl Clone + Copy) {
    &1u8
}

pub fn raw_pointer_opaque_return() -> *const (impl Clone + Copy) {
    std::ptr::null::<u8>()
}

pub fn fn_bound_pointer_to_dyn_return() -> impl Fn() -> *const (dyn Send + Sync) + Clone {
    || {
        static VALUE: u8 = 0;
        &VALUE as &(dyn Send + Sync) as *const (dyn Send + Sync)
    }
}

pub fn precise_capture_return<T: Clone>(value: T) -> impl Clone + use<T> {
    value
}

pub fn precise_capture_lifetime<'a, T: Clone + 'a>(
    value: &'a T,
) -> impl Clone + use<'a, T> {
    value
}

pub fn impl_trait_lifetime_return<'a>(value: &'a u8) -> impl Clone + 'a {
    value
}

pub fn precise_capture_const<const N: usize>(value: [u8; N]) -> impl Clone + use<N> {
    value
}

pub struct Example;

impl Example {
    pub fn add_method(&self, left: u64, right: u64) -> u64 {
        left + right
    }

    pub fn method_returns_nothing(&self) {}
}

pub trait MyTrait {
    fn add_trait_fn(&self, value: u64) -> u64;

    fn trait_fn_returns_nothing(&self);
}

pub trait FnOutputTrait {
    fn fn_output_dyn(&self) -> impl Fn() -> (dyn Send + Sync) + Clone;

    fn fn_output_dyn_single_bound(&self) -> impl Fn() -> (dyn Send + Sync);
}

pub struct GenericExample<Owner>(pub Owner);

impl<Owner> GenericExample<Owner> {
    pub fn combine<Method>(&self, owner: Owner, method: Method) -> (Owner, Method) {
        (owner, method)
    }

    pub fn pin_box_self(self: std::pin::Pin<Box<Self>>) {}
}

pub trait GenericTrait<TraitParam> {
    fn combine_trait<MethodParam>(
        &self,
        owner: TraitParam,
        method: MethodParam,
    ) -> (TraitParam, MethodParam);
}

pub trait Provider {
    type Assoc<T>;

    fn self_qualified(pair: (Self::Assoc<impl Clone>, impl Copy));
}

pub fn qualified_path_assoc_arg<T: Provider>(
    pair: (<T as Provider>::Assoc<impl Clone>, impl Copy),
) {
    let _ = pair;
}

pub fn generic_assoc_arg<T: Provider>(pair: (T::Assoc<impl Clone>, impl Copy)) {
    let _ = pair;
}

pub struct ImplementsGenericTrait<ImplParam>(pub ImplParam);

impl<ImplParam> GenericTrait<ImplParam> for ImplementsGenericTrait<ImplParam> {
    fn combine_trait<MethodParam>(
        &self,
        owner: ImplParam,
        method: MethodParam,
    ) -> (ImplParam, MethodParam) {
        (owner, method)
    }
}

pub trait DefaultGenericTrait<TraitParam> {
    fn default_combine<MethodParam>(
        &mut self,
        owner: TraitParam,
        method: MethodParam,
    ) -> (TraitParam, MethodParam) {
        (owner, method)
    }
}

pub struct UsesDefaultGenericTrait<ImplParam>(pub ImplParam);

impl<ImplParam> DefaultGenericTrait<ImplParam> for UsesDefaultGenericTrait<ImplParam> {}
