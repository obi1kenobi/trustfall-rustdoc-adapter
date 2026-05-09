#![allow(dead_code, private_bounds, private_interfaces)]

use core::{marker::PhantomData, mem::ManuallyDrop};

// Local public trait used in direct bounds, `where` bounds,
// nested associated type constraints, and qualified path predicates.
pub trait PublicTrait {
    type Assoc;
}

pub trait GenericPublicTrait<T> {
    type Assoc;
}

pub trait PublicLendingTrait<'borrow> {
    type Item;
}

// Local public trait with a module path and a root re-export. This exercises the
// adapter's implementation-defined "first importable path" normalization.
pub mod visible {
    pub trait ModuleTrait {
        type Item;
    }

    pub struct PublicType;
}
pub use visible::ModuleTrait as ReexportedModuleTrait;

// These items are local but not publicly importable. Bounds that reference them
// exercise the implementation-defined private-item normalized path fallback.
mod private {
    pub trait PrivateTrait {}

    pub struct PrivateType;
}

// No generics and no bounds should normalize to `<>`.
pub struct NoGenerics;

// There are no generic parameters here, but the concrete type predicate should
// still appear in `where` position after the leading empty generic list.
pub struct NonGenericTrivialBounds
where
    String: Clone;

// Similar to `NonGenericTrivialBounds`, but uses both `std` and `core` paths on
// concrete types so non-generic `where` predicates get normalized.
pub struct NonGenericConcreteBounds
where
    std::vec::Vec<u8>: Default,
    &'static str: AsRef<str>;

// Lifetimes, type parameters, const parameters, defaults, and a direct `?Sized`
// bound are all normalized and the bound is moved into `where` position.
pub struct SimpleGenerics<'original, TypeParam: ?Sized, const COUNT: usize = 3> {
    marker: PhantomData<(&'original TypeParam, [(); COUNT])>,
}

// Direct type and lifetime bounds, plus a defaulted const generic,
// results in the bounds moved into `where` position.
pub struct DirectBounds<'long, T: Clone + 'long, const N: usize = 0> {
    marker: PhantomData<(&'long T, [(); N])>,
}

// Same logical bounds as `DirectBounds`, but already written in `where`
// position. This checks that direct and `where`-clause spellings normalize
// consistently.
pub struct WhereBounds<'renamed, T, const N: usize = 0>
where
    T: Clone + 'renamed,
{
    marker: PhantomData<&'renamed T>,
}

// A lifetime bound written directly in the generic parameter list is moved into
// `where` position and lifetime names are normalized by definition order.
pub struct LifetimeParamBounds<'short, 'long: 'short> {
    marker: PhantomData<&'long &'short ()>,
}

// Lifetime predicate subjects are sorted by normalized lifetime name.
pub struct LifetimeSubjectOrdering<'zulu, 'alpha, 'middle>
where
    'middle: 'alpha,
    'zulu: 'middle,
{
    marker: PhantomData<&'zulu &'middle &'alpha ()>,
}

// Split and combined lifetime predicates should normalize identically.
pub struct LifetimePredicateMergeSplit<'first, 'second, 'third>
where
    'third: 'first,
    'third: 'second,
{
    marker: PhantomData<fn(&'first (), &'second (), &'third ())>,
}

pub struct LifetimePredicateMergeCombined<'first, 'second, 'third>
where
    'third: 'first + 'second,
{
    marker: PhantomData<fn(&'first (), &'second (), &'third ())>,
}

// Type parameter defaults are not bounds, so they stay in the generic parameter
// list while any paths inside the default are normalized.
pub struct DefaultType<T = visible::PublicType> {
    marker: PhantomData<T>,
}

// Definition order across lifetimes, types, and consts is preserved while each
// category gets its own normalized name sequence.
pub enum DefinitionOrder<'first, 'second, A, const N: usize, B, const M: usize = 7>
where
    'second: 'first,
    A: PublicTrait<Assoc = B>,
    B: Default,
{
    Variant(PhantomData<(&'first A, &'second B, [(); N], [(); M])>),
}

// Top-level `where` predicates are intentionally written out of normalized order.
// They should normalize as: generic subjects, associated item subjects, other
// generic-dependent subjects, lifetime subjects, and finally trivial subjects.
pub struct BoundSubjectOrdering<'later, 'earlier, A, B>
where
    String: Clone,
    &'static str: AsRef<str>,
    'later: 'earlier,
    (A, B): Clone,
    <B as PublicTrait>::Assoc: Default,
    B: Default + PublicTrait,
    A: core::fmt::Debug + Clone,
{
    marker: PhantomData<(&'later A, &'earlier B)>,
}

// Composite generic-dependent subjects sort lexicographically by their fully
// normalized subject text. This pins down how tuple subjects compare against
// array subjects that contain normalized const parameters.
pub struct CompositeSubjectOrdering<A, B, const N: usize, const M: usize>
where
    [B; M]: Copy,
    [A; N]: Clone,
    (A, B): Default,
{
    marker: PhantomData<(A, B, [(); N], [(); M])>,
}

// Const generic arguments inside paths are normalized in the same `C1`, `C2`,
// etc. namespace as const parameters in array subjects and defaults.
pub struct ConstArgTarget<const N: usize>;

// These two `struct` items spell the same bounds in different orders. Both should
// normalize to the same signature, with trait bounds sorted before lifetimes.
pub struct BoundComponentOrderA<'life, T>
where
    T: Clone + core::fmt::Debug + 'life,
{
    marker: PhantomData<&'life T>,
}

pub struct BoundComponentOrderB<'life, T>
where
    T: 'life + core::fmt::Debug + Clone,
{
    marker: PhantomData<&'life T>,
}

// Direct generic parameter bounds and `where`-clause bounds on the same subject
// should merge before sorting the subject's bound components.
pub struct SplitSubjectBounds<T: Clone>
where
    T: core::fmt::Debug + Default,
{
    marker: PhantomData<T>,
}

// One-element tuples need the trailing comma in both predicate subjects and
// nested type arguments, otherwise they become parenthesized non-tuple types.
pub struct OneElementTupleBounds<T, U>
where
    (T,): Clone,
    U: AsRef<(T,)>,
{
    marker: PhantomData<(T, U)>,
}

// Angle-bracketed generic arguments can include lifetimes, types containing
// const parameters, and const arguments.
pub struct GenericArgumentFlavors<'borrow, T, const N: usize>
where
    T: AsRef<std::borrow::Cow<'borrow, [u8; N]>> + AsRef<ConstArgTarget<N>>,
{
    marker: PhantomData<&'borrow T>,
}

// `fn` pointer types can appear both as predicate subjects and as nested
// generic arguments. The normalized form should not include placeholders
// for the function parameters.
pub struct FunctionPointerBounds<T>
where
    fn(T) -> T: Clone,
    T: AsRef<fn(T) -> T>,
{
    marker: PhantomData<T>,
}

// Function pointer ABIs and `unsafe` qualifiers can appear in predicate
// subjects and nested generic arguments. `const fn` and `async fn` are not
// function pointer type syntax.
pub struct FunctionPointerQualifierBounds<T>
where
    extern "C" fn(T) -> T: Clone,
    extern "C-unwind" fn(T) -> T: Clone,
    extern "system" fn(T) -> T: Clone,
    extern "sysv64" fn(T) -> T: Clone,
    unsafe fn(T) -> T: Clone,
    unsafe extern "C" fn(T) -> T: Clone,
    T: AsRef<unsafe extern "C" fn(T) -> T>,
{
    marker: PhantomData<T>,
}

// Local public, re-exported local, private local, `std`/`core`, and external-crate
// paths all appear in one bound list to pin down normalized path behavior.
pub struct LocalAndForeignBounds<T>
where
    T: PublicTrait
        + ReexportedModuleTrait
        + private::PrivateTrait
        + core::fmt::Debug
        + std::hash::Hash
        + equivalent::Equivalent<str>,
{
    marker: PhantomData<T>,
}

// These three items express the same logical `IntoIterator` constraints in
// different stable Rust forms. The `Item = ...` equality remains in the
// `IntoIterator` bound, while `Item: ...` and `IntoIter: ...` bounds normalize
// into explicit projection predicates.
pub struct AssociatedConstraintFormsA<T>
where
    T: IntoIterator<Item = visible::PublicType, IntoIter: ExactSizeIterator>,
    <T as IntoIterator>::Item: PublicTrait<Assoc = private::PrivateType>,
    <T as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    marker: PhantomData<T>,
}

// The same logical constraints as `AssociatedConstraintFormsA`, written only as
// associated item constraints on the `IntoIterator` bound.
pub struct AssociatedConstraintFormsB<T>
where
    T: IntoIterator<
            Item = visible::PublicType,
            Item: PublicTrait<Assoc = private::PrivateType>,
            IntoIter: ExactSizeIterator + DoubleEndedIterator,
        >,
{
    marker: PhantomData<T>,
}

// The same logical constraints as `AssociatedConstraintFormsA`, with associated
// item bounds written only as explicit projection predicates. The equality
// constraint remains nested because top-level equality constraints are unstable.
pub struct AssociatedConstraintFormsC<T>
where
    T: IntoIterator<Item = visible::PublicType>,
    <T as IntoIterator>::Item: PublicTrait<Assoc = private::PrivateType>,
    <T as IntoIterator>::IntoIter: ExactSizeIterator + DoubleEndedIterator,
{
    marker: PhantomData<T>,
}

// Multiply-nested associated item bounds recursively normalize into projection
// predicates for `<T as PublicTrait>::Assoc` and then for its own `Assoc` item.
pub struct MultiplyNestedAssociatedBounds<T>
where
    T: PublicTrait<Assoc: PublicTrait<Assoc: Clone>>,
{
    marker: PhantomData<T>,
}

// This associated item subject has a projection inside the trait path's type
// argument. It should still sort as a shallow projection, rather than as if the
// subject itself were multiply nested.
pub struct ParallelProjectionSubject<T>
where
    T: PublicTrait + GenericPublicTrait<<T as PublicTrait>::Assoc>,
    <T as PublicTrait>::Assoc: PublicTrait,
    <T as GenericPublicTrait<<T as PublicTrait>::Assoc>>::Assoc: Clone,
    <<T as PublicTrait>::Assoc as PublicTrait>::Assoc: Default,
{
    marker: PhantomData<T>,
}

// Higher-ranked binders also apply to recursively normalized projection
// predicates for `<T as PublicLendingTrait>::Item` and then for its own `Assoc`
// item.
pub struct HigherRankedNestedAssociatedBounds<T>
where
    T: for<'borrow> PublicLendingTrait<
            'borrow,
            Item: PublicTrait<Assoc: AsRef<&'borrow visible::PublicType>>,
        >,
{
    marker: PhantomData<T>,
}

// The higher-ranked binder can belong to the `where` predicate rather than the
// trait bound. Lifted associated-bound predicates must retain that binder.
pub struct HigherRankedWherePredicateAssociatedBounds<T>
where
    for<'borrow> T: PublicLendingTrait<
            'borrow,
            Item: PublicTrait<Assoc: AsRef<&'borrow visible::PublicType>>,
        >,
{
    marker: PhantomData<T>,
}

// If a lifted predicate has both the enclosing `where`-predicate binder and its
// own trait-bound binder, the binders should be combined instead of dropping one.
pub struct HigherRankedWherePredicateNestedAssociatedBounds<T>
where
    for<'outer> T: PublicLendingTrait<
            'outer,
            Item: for<'inner> PublicLendingTrait<
                'inner,
                Item: AsRef<(&'outer visible::PublicType, &'inner visible::PublicType)>,
            >,
        >,
{
    marker: PhantomData<T>,
}

// Higher-ranked trait bounds normalize their own local lifetime names and also
// normalize local public and private types inside a parenthesized `Fn` bound.
pub struct HigherRanked<F>
where
    F: for<'borrow> Fn(&'borrow visible::PublicType) -> &'borrow private::PrivateType,
{
    marker: PhantomData<F>,
}

// `'_` is rejected in ordinary type paths in `where` clauses, but is accepted
// in parenthesized `Fn` bounds and rustdoc reports it as an elided borrowed-ref
// lifetime.
pub struct AnonymousLifetimeInFnBound<T>
where
    T: Fn(&'_ visible::PublicType) -> &'_ private::PrivateType,
{
    marker: PhantomData<T>,
}

// Higher-ranked lifetimes and outer owner lifetimes can appear in the same
// bound. Their normalized names must not collide.
pub struct HigherRankedWithOuterLifetime<'outer, F>
where
    F: for<'borrow> Fn(&'borrow visible::PublicType, &'outer visible::PublicType),
{
    marker: PhantomData<&'outer F>,
}

// Nested higher-ranked binders should keep assigning fresh normalized lifetime
// names as formatting recurses.
pub struct NestedHigherRankedFreshNames<F>
where
    F: for<'first, 'second> Fn(for<'third> fn(&'third ()), &'first (), &'second ()),
{
    marker: PhantomData<F>,
}

// Nested `dyn Trait` types inside generic arguments normalize their trait
// paths, associated item equality constraints, auto-trait bounds, and lifetime
// bounds.
pub struct DynTraitWhere<'env, T>
where
    T: AsRef<dyn PublicTrait<Assoc = visible::PublicType> + Send + 'env>,
{
    marker: PhantomData<&'env T>,
}

// Qualified paths on the left-hand side of a `where` predicate should rewrite the
// trait path to its normalized spelling and normalize generic type names inside
// the projection.
pub struct QualifiedPathBounds<T, U>
where
    T: PublicTrait<Assoc = U>,
    <T as PublicTrait>::Assoc: equivalent::Equivalent<visible::PublicType>,
{
    marker: PhantomData<(T, U)>,
}

// Const generic names are normalized in generic parameter lists and in simple
// const positions such as array lengths.
pub struct ConstBounds<T, const N: usize, const M: usize = 4>
where
    T: AsRef<[u8; N]>,
    [u8; N]: Default,
{
    marker: PhantomData<(T, [u8; N], [u8; M])>,
}

// Concrete const expressions are currently preserved according to today's
// rustdoc JSON string representation. Today, rustdoc preserves the const default
// as `{ 1 + 2 }`, evaluates the array length to `3`, and emits `{ _ }` for the
// const generic argument. This test should fail if rustdoc starts emitting a
// different representation that we may want to normalize.
pub struct ConcreteConstExpr<const N: usize = { 1 + 2 }>
where
    [u8; 1 + 2]: Default,
    ConstArgTarget<{ 1 + 2 }>: Sized,
{
    marker: PhantomData<[(); N]>,
}

// Type and const parameters can interleave in definition order. Type names and
// const names use separate normalized sequences without changing that order.
pub struct InterleavedTypeConstParams<A, const B: usize, C, const D: usize>
where
    A: AsRef<[C; B]>,
    [C; D]: Clone,
{
    marker: PhantomData<fn(A, C) -> ([(); B], [(); D])>,
}

// Non-generic types in `where` predicates, including tuples and raw pointers, are
// preserved while generic names and trait paths inside them are normalized.
pub struct NonGenericWhereTypes<T>
where
    (T, *const T): Clone,
{
    marker: PhantomData<T>,
}

// A generic enum is an `ImplOwner` too, so it should use the same
// normalization as `struct` items while still excluding the enum name itself.
pub enum EnumOwner<'enum_lt, T, const N: usize>
where
    T: PublicTrait + 'enum_lt,
    [(); N]: Sized,
{
    Variant(PhantomData<&'enum_lt T>),
}

// A generic union is the third `ImplOwner` implementor. The `ManuallyDrop` field
// avoids placing extra `Copy` constraints on the field type itself.
pub union UnionOwner<T, const N: usize>
where
    T: private::PrivateTrait,
    [u8; N]: Copy,
{
    value: ManuallyDrop<T>,
}

pub mod renamed {
    use core::marker::PhantomData;

    // The public re-export below renames this item. The normalized signature
    // must exclude the owner name so the re-export name cannot affect it.
    pub struct OriginalName<T: super::PublicTrait> {
        marker: PhantomData<T>,
    }
}
pub use renamed::OriginalName as RenamedForImport;
