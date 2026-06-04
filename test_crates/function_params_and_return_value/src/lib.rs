pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn fn_returns_nothing() {}

pub struct PublicType<T>(pub T);

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

pub fn impl_trait_return() -> impl Iterator<Item = u8> {
    0u8..=1
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
