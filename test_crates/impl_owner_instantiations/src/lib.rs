#![feature(sized_hierarchy)]

use core::marker::{MetaSized, PhantomData, PointeeSized};

#[derive(Clone, Copy)]
pub enum Plain {
    Unit,
}

impl Plain
where
    Plain: Copy,
{
    pub fn plain_where_copy() {}
}

pub trait Decode {}

impl Decode for Plain {}

pub struct Reader<T>(pub T);

impl Reader<u8> {
    pub fn byte_reader() {}
}

impl<U> Reader<Vec<U>> {
    pub fn vector_reader() {}
}

impl Reader<()> {
    pub fn unit_reader() {}
}

impl<U> Reader<&U> {
    pub fn reference_reader() {}
}

impl<U: Copy> Reader<U> {
    pub fn copy_reader() {}
}

pub mod local {
    pub trait Sized {}
    impl Sized for u8 {}
}

impl<U: local::Sized> Reader<U> {
    pub fn locally_sized_reader() {}
}

pub trait Byte {
    type Value;
}

impl<T> Byte for T {
    type Value = T;
}

impl<T: Byte> Reader<T> {
    pub fn blanket_byte_reader() {}
}

pub trait CopyMarker: Copy {}

// The unused lifetime places no bound on `T`. This blanket impl still proves
// `CopyMarker` for every `T: Copy`.
impl<'unused, T: Copy> CopyMarker for T {}

impl<T: CopyMarker> Reader<T> {
    pub fn conditional_blanket_reader() {}
}

pub trait MarkerFor<V = u8> {}
impl<T> MarkerFor<u16> for T {}
impl MarkerFor<u8> for u8 {}

// The blanket impl supplies `MarkerFor<u16>`, while this bound selects the default `u8`.
impl<T: MarkerFor> Reader<T> {
    pub fn default_marker_reader() {}
}

pub struct CopyReader<T: Copy>(pub T);

impl<T: Copy + CopyMarker> CopyReader<T> {
    pub fn owner_already_requires_copy() {}
}

pub struct StaticReader<T: 'static>(pub T);

// Every legal `T` is `'static` and therefore implements `Any`. This crate's rustdoc
// omits the trait definition and the blanket impl declaration from `core`,
// so the query returns `null` for this impl.
impl<T: core::any::Any> StaticReader<T> {
    pub fn external_blanket_reader() {}
}

// The blanket `Byte` impl makes `u8: Byte<Value = T>` require `T = u8`.
// The fixed impl below covers that sole legal argument. The adapter cannot
// prove this from a bound on `u8`.
pub struct OnlyByte<T>(pub T)
where
    u8: Byte<Value = T>;

impl OnlyByte<u8> {
    pub fn only_byte() {}
}

// Here the bound is on `T` itself. Proving that only `u8` is legal requires
// using the blanket impl's `Value = T` to resolve the inline constraint `Value = u8`.
pub struct OnlyByteInline<T: Byte<Value = u8>>(pub T);

impl OnlyByteInline<u8> {
    pub fn only_byte_inline() {}
}

pub struct Mapped<T: Byte>(pub T);

impl<T: Byte> Mapped<T> {
    pub fn any_associated_value() {}
}

impl<T: Byte<Value = u8>> Mapped<T> {
    pub fn requires_byte_value() {}
}

pub struct Pair<A, B = u8>(pub A, pub B);

impl<X, Y> Pair<Y, X> {
    pub fn swapped_parameters() {}
}

impl<X> Pair<X, X> {
    pub fn repeated_parameter() {}
}

impl<X> Pair<X, u8> {
    pub fn fixed_second_parameter() {}
}

impl<X> Pair<X> {
    pub fn default_second_parameter() {}
}

impl<X, Y> Pair<Vec<X>, Y> {
    pub fn wrapped_first_parameter() {}
}

impl<X, Y: Copy> Pair<X, Y> {
    pub fn copy_second_parameter() {}
}

pub trait RelatedTo<T> {}
impl RelatedTo<u16> for u8 {}
impl RelatedTo<u8> for u8 {}
impl RelatedTo<u8> for u16 {}
impl RelatedTo<u32> for u8 {}

pub struct Related<A, B>(pub A, pub B)
where
    A: RelatedTo<B>;

impl<X, Y: RelatedTo<X>> Related<Y, X> {
    pub fn matching_relation() {}
}

impl<X, Y: RelatedTo<X>> Related<Y, X>
where
    X: RelatedTo<Y>,
{
    pub fn additional_reverse_relation() {}
}

// `Related<u16, u8>` meets the owner's bound, but `u16: RelatedTo<u16>` does not hold.
impl<X, Y: RelatedTo<X> + RelatedTo<Y>> Related<Y, X> {
    pub fn also_requires_relation_to_self() {}
}

pub union Widths<const N: usize, const M: usize> {
    pub first: [u8; N],
    pub second: [u8; M],
}

impl<const A: usize, const B: usize> Widths<B, A> {
    pub fn independent_widths() {}
}

pub type Width = usize;

impl<const A: Width, const B: usize> Widths<A, B> {
    pub fn aliased_const_parameter_type() {}
}

impl<const N: usize> Widths<N, N> {
    pub fn equal_widths() {}
}

pub struct View<'a, 'b>(pub &'a u8, pub &'b u8);

impl View<'_, '_> {
    pub fn independent_lifetimes() {}
}

impl<'a> View<'a, 'a> {
    pub fn equal_lifetimes() {}
}

impl View<'static, '_> {
    pub fn static_lifetime() {}
}

// The next two impls accept any choice of `'unused`.
impl<'unused: 'unused> View<'_, '_> {
    pub fn reflexive_unmapped_lifetime_bound() {}
}

impl<'unused> View<'_, '_>
where
    'static: 'unused,
{
    pub fn static_outlives_unmapped_lifetime() {}
}

// These bounds require `'a: 'b` through the intermediate lifetime.
impl<'a: 'middle, 'middle: 'b, 'b> View<'a, 'b> {
    pub fn requires_outlives_through_middle() {}
}

pub struct Reborrow<'long, 'short>(pub &'short &'long u8)
where
    &'long u8: 'short;

// The impl accepts every legal reborrow. Rustdoc records the owner's outlives
// bound on a reference type, which the predicate comparison cannot represent.
impl<'a, 'b> Reborrow<'a, 'b> {
    pub fn every_reborrow() {}
}

pub trait Marked<'a, const N: usize> {}
impl<'a, const N: usize> Marked<'a, N> for u8 {}

pub struct Window<'short, 'long: 'short, T: 'long, const N: usize>(pub &'short (), pub &'long T)
where
    T: Marked<'short, N>;

impl<'a, 'b, U, const K: usize> Window<'a, 'b, U, K>
where
    'b: 'a,
    U: 'b + Marked<'a, K>,
{
    pub fn matching_outlives_and_trait_arguments() {}
}

impl<'a: 'b, 'b: 'a, U: 'b + Marked<'a, K>, const K: usize> Window<'a, 'b, U, K> {
    pub fn also_requires_reverse_lifetime() {}
}

pub struct Buffer<'a, T, const N: usize, U>(pub &'a [T; N], pub U);

impl<'b, V, W, const M: usize> Buffer<'b, W, M, V> {
    pub fn mixed_parameters() {}
}

impl<'a, T, const N: usize> Buffer<'a, T, N, u8> {
    pub fn fixed_buffer_argument() {}
}

impl<'a, T, U> Buffer<'a, T, 2, U> {
    pub fn two_elements() {}
}

pub trait HasValue<V>: Byte<Value = V> {}
impl<T> HasValue<T> for T {}

// The blanket `Byte` impl sets `Value = Self` for sized types, so `T: HasValue<U>`
// forces `U = T`. Proving that the impl with repeated `T` covers every legal pair
// would require inferring this equality from the associated type.
pub struct MatchingBuffer<'a, T: HasValue<U>, Tag, U, const N: usize = 1> {
    pub data: &'a [T; N],
    pub tag: Tag,
    pub value: U,
}

impl<'a, T, Tag, const N: usize> MatchingBuffer<'a, T, Tag, T, N> {
    pub fn matching_buffer_arguments() {}
}

// `Tag` can vary independently of `T` and `U`. Reusing either parameter
// in the tag position excludes buffers with a different tag type.
impl<'a, T: HasValue<U>, U, const N: usize> MatchingBuffer<'a, T, T, U, N> {
    pub fn tag_matches_first() {}
}

impl<'a, T: HasValue<U>, U, const N: usize> MatchingBuffer<'a, T, U, U, N> {
    pub fn tag_matches_last() {}
}

// The `HasValue<U>` bound leaves `N` free to vary. Omitting `N` fixes it to the
// default of `1`, excluding buffers of length `2`.
impl<'a, T: HasValue<U>, Tag, U> MatchingBuffer<'a, T, Tag, U> {
    pub fn default_buffer_length() {}
}

pub struct MaybeSized<T: ?Sized>(pub T);

impl<U> MaybeSized<U>
where
    U: ?Sized,
{
    pub fn also_for_str() {}
}

impl<U> MaybeSized<U> {
    pub fn sized_only() {}
}

impl<U: ?Sized + Byte> MaybeSized<U> {
    pub fn sized_blanket() {}
}

pub trait UnrestrictedMarker {}
impl<T: ?Sized> UnrestrictedMarker for T {}

impl<U: ?Sized + UnrestrictedMarker> MaybeSized<U> {
    pub fn unsized_blanket() {}
}

impl<T> Reader<T>
where
    T: Sized,
{
    pub fn explicitly_sized() {}
}

pub fn check_sized_and_unsized_instantiations() {
    MaybeSized::<u8>::also_for_str();
    MaybeSized::<u8>::sized_only();
    MaybeSized::<str>::also_for_str();
}

// The explicit `Sized` bound restores the requirement relaxed by `?Sized`,
// so the implicitly sized impl below covers every legal argument.
pub struct RestoredSized<T: ?Sized>(pub T)
where
    T: Sized;

// The unused lifetime has no position in the self type and adds no bound
// for the comparison with the owner's requirements.
impl<'unused, U> RestoredSized<U> {
    pub fn requires_sized() {}
}

pub struct ForMetaSized<T: MetaSized>(pub PhantomData<T>);

impl<T> ForMetaSized<T> {
    pub fn narrowed_meta_sized() {}
}

pub struct ForPointeeSized<T: PointeeSized>(pub PhantomData<T>);

impl<T> ForPointeeSized<T> {
    pub fn narrowed_pointee_sized() {}
}

impl<T: ?Sized> ForPointeeSized<T> {
    pub fn relaxed_pointee_sized() {}
}

pub const ONE: usize = 1;
pub const TWO: usize = 2;

pub trait Bound<const N: usize> {}
impl<T> Bound<1> for T {}
impl Bound<2> for u8 {}

// `ConstBound<u16>` is legal, but `u16` lacks the `Bound<2>` required by the impl.
// Rustdoc 1.102.0-nightly (2026-11-11) prints both const expressions as `{ _ }`.
// Comparing their spelling would incorrectly report that the impl covers every `ConstBound<T>`.
pub struct ConstBound<T: Bound<{ 0 + ONE }>>(pub T);

impl<T: Bound<{ 0 + TWO }>> ConstBound<T> {
    pub fn different_const_bounds() {}
}
