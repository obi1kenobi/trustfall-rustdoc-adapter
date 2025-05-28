use core::marker::Sized as SizedRenamed;

// A trait used to guard against bugs due to name-based matching
// for the built-in `Sized` trait when determining `?Sized` status.
pub trait Sized {
    fn method();
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub struct GenericStruct<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    x: Box<T>,
    y: U,
    z: V,
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
pub union GenericUnion<T: ?core::marker::Sized + Copy, U: core::marker::Sized, V> {
    field: std::mem::ManuallyDrop<Box<T>>,
    field2: std::mem::ManuallyDrop<U>,
    field3: std::mem::ManuallyDrop<V>,
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub trait GenericTrait<T: ?core::marker::Sized, U: core::marker::Sized, V> {
    // `W` is maybe-sized.
    // `X` and `Y` are sized.
    fn trait_method<W: ?core::marker::Sized, X: core::marker::Sized, Y>(value: &Box<T>);
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub fn generic_fn1<T: ?core::marker::Sized, U: core::marker::Sized, V>(x: &T) {}

// `T` is sized. The `?Sized` is overridden by the explicit bound.
pub fn generic_fn2<T: core::marker::Sized + ?core::marker::Sized>(value: &T) {}

// `T` is maybe-sized. The `Sized` here is the unrelated trait, not the built-in one.
pub fn generic_fn3<T: Sized + ?core::marker::Sized>(value: T) {}

// `T` is sized. `SizedRenamed` is an import rename of the built-in `Sized` trait.
pub fn generic_f3a<T: SizedRenamed + ?core::marker::Sized>(value: T) {}

// `T` is sized. The `?Sized` is overridden by both explicit bounds: the `where` and the `:` bound.
pub fn generic_fn4<T: core::marker::Sized + ?core::marker::Sized>(value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized. The `?Sized` is overridden by both explicit bounds: the `where` and the `:` bound.
pub fn generic_fn5<T: ?core::marker::Sized + core::marker::Sized>(value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized. The `?Sized` is overridden by the explicit bound in the `where` clause.
pub fn generic_fn6<T: ?core::marker::Sized>(value: &T)
where
    T: core::marker::Sized,
{
}

// `T` is sized due to the bound in the `:` clause.
pub fn generic_fn7<T: core::marker::Sized>(value: &T)
where
    T: ?core::marker::Sized,
{
}

// `T` is maybe-sized due to the `where` clause.
pub fn generic_fn7a<T>(value: &T)
where
    T: ?core::marker::Sized,
{
}

// `T` is maybe-sized.
// `U` and `V` are sized.
pub fn generic_fn8<T, U, V>(value: &T)
where
    T: ?core::marker::Sized,
    U: core::marker::Sized,
{
}

// `T` is maybe-sized.
pub fn generic_fn9<T>(value: &T)
where
    T: ?SizedRenamed,
{
}

// `T` is maybe-sized.
// `U` and `V` are sized.
// The synthetic generic from the `impl Trait` is sized.
pub fn impl_trait<T: ?core::marker::Sized, U: core::marker::Sized, V>(
    value: impl GenericTrait<T, U, V>,
) {
}

// `T` is maybe-sized.
// `U` and `V` are sized.
// The synthetic generic from the `impl Trait` is maybe-sized.
pub fn impl_trait2<T: ?core::marker::Sized, U: core::marker::Sized, V>(
    value: impl GenericTrait<T, U, V> + ?core::marker::Sized,
) {
}

pub struct ExampleStruct;

impl ExampleStruct {
    // `T` is maybe-sized.
    // `U` is sized.
    pub fn generic_method<T: ?SizedRenamed, U>(left: &T, right: &U) {}
}

// `T` is maybe-sized here.
pub struct ImplNarrowing<T: ?core::marker::Sized>(Box<T>);

impl<T> ImplNarrowing<T> {
    // Here `T` is sized because the `impl` didn't say it was `?Sized`.
    pub fn taking_sized_t(value: T) {}
}
