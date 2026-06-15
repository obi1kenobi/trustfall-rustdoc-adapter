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

pub trait TakesLifetimeConst<'a, const N: usize> {}

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
    type Item;
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
