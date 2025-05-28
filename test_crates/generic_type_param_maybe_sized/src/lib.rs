#![deny(clippy::needless_maybe_sized)]

use core::marker::Sized as SizedRenamed;

// A trait used to guard against bugs due to name-based matching
// for the built-in `Sized` trait when determining `?Sized` status.
pub trait Sized {
    fn method();
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub struct GenericStruct<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    _x: Box<T>,
    _y: U,
    _z: V,
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub enum GenericEnum<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    Variant(Box<T>),
    Variant2(U),
    Variant3(V),
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub union GenericUnion<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    _field: std::mem::ManuallyDrop<Box<T>>,
    _field2: std::mem::ManuallyDrop<U>,
    _field3: std::mem::ManuallyDrop<V>,
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub trait GenericTrait<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    // `W` is maybe-sized.
    // `X` and `Y` are sized.
    fn trait_method<W: ?core::marker::Sized, X: core::marker::Sized, Y>(_value: &T);
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub fn generic_fn1<T: ?core::marker::Sized, U: core::marker::Sized, V>(_value: &T) {}

// `T` is sized. The `?Sized` is overridden by the explicit bound on `Sized`.
#[allow(clippy::needless_maybe_sized)]
pub fn generic_fn2<T: core::marker::Sized + ?core::marker::Sized>(_value: &T) {}

// `T` is maybe-sized. The `Sized` here is the unrelated trait, not the built-in one.
pub fn generic_fn3<T: Sized + ?core::marker::Sized>(_value: &T) {}

// `T` is sized. `SizedRenamed` is an import rename of the built-in `Sized` trait.
#[allow(clippy::needless_maybe_sized)]
pub fn generic_fn3a<T: SizedRenamed + ?core::marker::Sized>(_value: T) {}

// `T` is sized. The `?Sized` is overridden by both explicit bounds: the `where` and the `:` bound.
#[allow(clippy::needless_maybe_sized)]
#[allow(clippy::multiple_bound_locations)]
pub fn generic_fn4<T: core::marker::Sized + ?core::marker::Sized>(_value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized. The `?Sized` is overridden by both explicit bounds: the `where` and the `:` bound.
#[allow(clippy::needless_maybe_sized)]
#[allow(clippy::multiple_bound_locations)]
pub fn generic_fn5<T: ?core::marker::Sized + core::marker::Sized>(_value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized. The `?Sized` is overridden by the explicit bound in the `where` clause.
#[allow(clippy::needless_maybe_sized)]
#[allow(clippy::multiple_bound_locations)]
pub fn generic_fn6<T: ?core::marker::Sized>(_value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized due to the bound in the `:` clause.
#[allow(clippy::needless_maybe_sized)]
#[allow(clippy::multiple_bound_locations)]
pub fn generic_fn7<T: core::marker::Sized>(_value: &T)
where
    T: ?core::marker::Sized,
{
}

// `T` is maybe-sized due to the `where` clause.
pub fn generic_fn7a<T>(_value: &T)
where
    T: ?core::marker::Sized,
{
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub fn generic_fn8<T, U, V>(_value: &T)
where
    T: ?core::marker::Sized,
    U: core::marker::Sized,
{
}

// `T` is maybe-sized.
pub fn generic_fn9<T>(_value: &T)
where
    T: ?SizedRenamed,
{
}

// `T` is maybe-sized.
// `U` and `V` are sized.
// The synthetic generic from the `impl Trait` is sized.
pub fn impl_trait<T: ?core::marker::Sized, U: core::marker::Sized, V>(
    _value: impl GenericTrait<T, U, V>,
) {
}

// `T` is maybe-sized.
// `U` and `V` are sized.
// The synthetic generic from the `impl Trait` is maybe-sized.
pub fn impl_trait2<T: ?core::marker::Sized, U: core::marker::Sized, V>(
    _value: &(impl GenericTrait<T, U, V> + ?core::marker::Sized),
) {
}

pub struct ExampleStruct;

impl ExampleStruct {
    // `T` is maybe-sized.
    // `U` is sized.
    pub fn generic_method<T: ?SizedRenamed, U>(_left: &T, _right: &U) {}
}

// `T` is maybe-sized here.
pub struct ImplNarrowing<T: ?core::marker::Sized>(Box<T>);

impl<T> ImplNarrowing<T> {
    // Here `T` is sized because the `impl` didn't say it was `?Sized`.
    pub fn taking_sized_t(_value: T) {}
}

// Anything that implements this trait must be `Sized`.
trait SizedSuper: Sized {}

// `T` here is a weird edge case:
// - A *local* analysis of the bounds determines it to be `?Sized`.
//   Our `maybe_sized` boolean property will say `true` because of this.
// - However, a *global* analysis could determine that `T: SizedSuper` implies `T: Sized`.
//   This is a fact that downstream users of `ImplicitlySized` can depend on,
//   but is entirely non-obvious from the public API that this is the case.
//
// Note that `SizedSuper` is a private trait. This is another example where
// knowledge of private items is required to determine the public API of a crate.
//
// This is also "infectious", auto-trait-like: a trait way upstream of `T`'s supertraits
// may change its bounds in a way that changes whether `T` is sized or not.
#[allow(private_bounds)]
pub struct ImplicitlySized<T: SizedSuper + ?core::marker::Sized>(T);

// `T` here is another such weird edge case:
// - A *local* analysis of the bounds determines it to be `?Sized`.
//   Our `maybe_sized` boolean property will say `true` because of this.
// - However, a *global* analysis could determine that `T: Copy` implies `T: Sized`.
//   This is a fact that downstream users of `ImplicitlySized` can depend on,
//   but is entirely non-obvious from the public API that this is the case.
//
// Furthermore, this shows that determining whether a type is *actually* `?Sized`
// requires cross-crate analysis. This is unfortunate.
#[allow(clippy::needless_maybe_sized)]
pub struct ImplicitlySizedFromBuiltInTrait<T: ?core::marker::Sized + Copy>(T);
