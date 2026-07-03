pub trait AssocConstraintOrder {
    type A;
    type B;
}

pub struct ConcreteAssoc;

impl AssocConstraintOrder for ConcreteAssoc {
    type A = u8;
    type B = u8;
}

pub trait Takes<T> {}
pub trait AlsoTakes<T> {}
pub trait TakesConst<const N: usize> {}
impl<T, U> Takes<T> for U {}
impl<T, U> AlsoTakes<T> for U {}
impl<T, const N: usize> TakesConst<N> for T {}

pub trait TakesLifetimeConst<'a, const N: usize> {}

pub trait RealTrait {
    fn method(&self);
}

pub struct DynAssoc;

pub struct DynPointerAssoc;

pub fn a_then_b(value: Box<dyn AssocConstraintOrder<A = impl Clone, B = impl Copy>>) {
    let _ = value;
}

pub fn b_then_a(value: Box<dyn AssocConstraintOrder<B = impl Copy, A = impl Clone>>) {
    let _ = value;
}

pub fn return_assoc_constraint_bound() -> impl AssocConstraintOrder<A: Clone + Copy, B = u8> {
    ConcreteAssoc
}

pub trait HasItem {
    type Item: ?Sized;
}

impl HasItem for ConcreteAssoc {
    type Item = u8;
}

impl HasItem for DynAssoc {
    type Item = dyn RealTrait + Send;
}

impl HasItem for DynPointerAssoc {
    type Item = fn() -> *const (dyn RealTrait + Send);
}

pub trait HasGenericItem {
    type Item<T>;
}

pub trait GenericAssoc<T> {
    type A;
}

impl HasGenericItem for ConcreteAssoc {
    type Item<T> = u8;
}

impl GenericAssoc<u8> for ConcreteAssoc {
    type A = u8;
}

pub fn return_generic_assoc_constraint_bound() -> impl GenericAssoc<u8, A: Clone + Copy> {
    ConcreteAssoc
}

pub fn return_gat_assoc_constraint_bound() -> impl HasGenericItem<Item<u8>: Clone + Copy> {
    ConcreteAssoc
}

pub fn return_nested_dyn_bound() -> impl HasItem<Item = dyn RealTrait + Send> {
    DynAssoc
}

pub fn return_nested_parenthesized_dyn_bound(
) -> impl HasItem<Item = fn() -> *const (dyn RealTrait + Send)> {
    DynPointerAssoc
}

pub fn return_nested_opaque_bound_clone_then_copy(
) -> impl HasItem<Item: Takes<impl Clone> + Takes<impl Copy>> {
    ConcreteAssoc
}

pub fn return_nested_opaque_bound_copy_then_clone(
) -> impl HasItem<Item: Takes<impl Copy> + Takes<impl Clone>> {
    ConcreteAssoc
}

pub fn return_function_pointer_bound_safe_then_unsafe(
) -> impl Takes<fn(u8) -> u8> + Takes<unsafe extern "C" fn(u8, ...) -> u8> {
}

pub fn return_function_pointer_bound_unsafe_then_safe(
) -> impl Takes<unsafe extern "C" fn(u8, ...) -> u8> + Takes<fn(u8) -> u8> {
}

pub fn return_assoc_constraints_b_then_a() -> impl AssocConstraintOrder<B = u8, A = u8> {
    ConcreteAssoc
}

pub fn return_const_arg_sort_key() -> impl TakesConst<2> + TakesConst<1> {}

pub fn return_function_pointer_signature_sort_key() -> impl Takes<fn(u8)> + Takes<fn(u16)> {}

pub fn return_function_pointer_header_sort_key() -> impl Takes<unsafe fn(u8)> + Takes<fn(u8)> {}

pub fn return_function_pointer_abi_sort_key(
) -> impl Takes<unsafe extern "C-unwind" fn(u8)> + Takes<unsafe extern "C" fn(u8)> {
}

pub trait Provider {
    type Assoc<T>;
}

pub fn bound_a_then_b<
    T: AssocConstraintOrder<A: HasItem<Item = impl Clone>, B: HasItem<Item = impl Copy>>,
>(
    value: T,
) {
    let _ = value;
}

pub fn bound_b_then_a<
    T: AssocConstraintOrder<B: HasItem<Item = impl Copy>, A: HasItem<Item = impl Clone>>,
>(
    value: T,
) {
    let _ = value;
}

pub fn same_synthetic_names(value: Box<dyn AssocConstraintOrder<A = impl Clone, B = impl Clone>>) {
    let _ = value;
}

pub fn gat_bound_u8_then_u16<
    T: HasGenericItem<Item<u8> = impl Clone, Item<u16> = impl Copy>,
>(
    value: T,
) {
    let _ = value;
}

pub fn repeated_gat_bound_u8_then_u16<
    T: HasGenericItem<Item<u8> = impl Clone> + HasGenericItem<Item<u16> = impl Copy>,
>(
    value: T,
) {
    let _ = value;
}

pub fn generic_bound_before_input_impl_trait<
    T: AssocConstraintOrder<A: HasItem<Item = impl Clone>>,
>(
    value: impl Send,
    other: T,
) {
    let _ = (value, other);
}

pub fn top_level_assoc_bound_counts_hidden_impls(
    pair: (
        impl AssocConstraintOrder<A: HasItem<Item = impl Clone>>,
        impl Copy,
    ),
) {
    let _ = pair;
}

pub fn assoc_constraint_args_count_before_outer_impl(
    pair: (
        impl HasGenericItem<Item<impl Clone> = impl Copy>,
        impl Default,
    ),
) {
    let _ = pair;
}

pub fn hrtb_bound_counts_before_later_impl(
    pair: (impl for<'a> Takes<&'a u8>, impl Clone),
) {
    let _ = pair;
}

pub fn hrtb_two_lifetime_bound_counts_before_later_impl(
    pair: (impl for<'a, 'b> Takes<(&'a u8, &'b u8)>, impl Clone),
) {
    let _ = pair;
}

pub fn lifetime_const_generic_arg_counts_before_later_impl<'a, const N: usize>(
    pair: (impl TakesLifetimeConst<'a, N>, impl Clone),
) {
    let _ = pair;
}

pub fn complex_constraint_a_then_b<'a>(
    value: Box<
        dyn AssocConstraintOrder<
            A = Box<dyn for<'b> Fn(&'b u8) -> &'b u8 + 'a>,
            B = impl Clone,
        >,
    >,
) {
    let _ = value;
}

pub fn complex_constraint_b_then_a<'a>(
    value: Box<
        dyn AssocConstraintOrder<
            B = impl Clone,
            A = Box<dyn for<'b> Fn(&'b u8) -> &'b u8 + 'a>,
        >,
    >,
) {
    let _ = value;
}

pub fn constraint_type_shapes<'a, const N: usize>(
    value: Box<dyn AssocConstraintOrder<A = (&'a [u8], [u8; N]), B = impl Clone>>,
) {
    let _ = value;
}

pub fn constraint_function_pointer(
    value: Box<dyn AssocConstraintOrder<A = fn(u8) -> u8, B = impl Clone>>,
) {
    let _ = value;
}

pub fn constraint_higher_ranked_function_pointer(
    value: Box<dyn AssocConstraintOrder<A = for<'a> fn(&'a u8) -> &'a u8, B = impl Clone>>,
) {
    let _ = value;
}

pub fn constraint_two_lifetime_function_pointer(
    value: Box<
        dyn AssocConstraintOrder<
            A = for<'a, 'b> fn(&'a u8, &'b u8) -> &'b u8,
            B = impl Clone,
        >,
    >,
) {
    let _ = value;
}

pub fn constraint_higher_ranked_two_constraints_b_then_a(
    value: Box<
        dyn AssocConstraintOrder<
            B = for<'b> fn(&'b u16) -> &'b u16,
            A = for<'a> fn(&'a u8) -> &'a u8,
        >,
    >,
) {
    let _ = value;
}

pub fn constraint_dyn_fn_trait_two_args(
    value: Box<dyn AssocConstraintOrder<A = Box<dyn Fn(u8, u16) -> u32>, B = impl Clone>>,
) {
    let _ = value;
}

pub fn constraint_qualified_path<T: Provider>(
    value: Box<dyn AssocConstraintOrder<A = <T as Provider>::Assoc<impl Clone>, B = impl Copy>>,
) {
    let _ = value;
}

pub fn where_bound_after_input_impl_trait<T>(value: impl Send, other: T)
where
    T: AssocConstraintOrder<A: HasItem>,
{
    let _ = (value, other);
}

pub fn constraint_multiple_bounds(
    value: impl AssocConstraintOrder<A: Clone + Copy, B = impl Default>,
) {
    let _ = value;
}

pub fn constraint_raw_pointers(
    value: Box<dyn AssocConstraintOrder<A = *const impl Clone, B = *mut impl Copy>>,
) {
    let _ = value;
}

pub fn constraint_slice_and_array(
    value: Box<dyn AssocConstraintOrder<A = &'static [impl Clone], B = [impl Copy; 3]>>,
) {
    let _ = value;
}

pub fn constraint_mutable_reference(
    value: Box<dyn AssocConstraintOrder<A = &'static mut impl Clone, B = impl Copy>>,
) {
    let _ = value;
}

pub fn constraint_single_tuple(
    value: Box<dyn AssocConstraintOrder<A = (impl Clone,), B = impl Copy>>,
) {
    let _ = value;
}

pub fn constraint_dyn_multi_trait<'a>(
    value: Box<dyn AssocConstraintOrder<A = Box<dyn Send + Sync + 'a>, B = impl Clone>>,
) {
    let _ = value;
}

pub fn constraint_dyn_multi_trait_reverse<'a>(
    value: Box<dyn AssocConstraintOrder<A = Box<dyn Sync + Send + 'a>, B = impl Clone>>,
) {
    let _ = value;
}
