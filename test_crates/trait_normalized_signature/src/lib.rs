#![allow(dead_code, private_bounds, private_interfaces)]

pub trait PublicTrait {
    type Assoc;
}

pub trait GenericPublicTrait<T> {
    type Assoc;
}

pub trait PublicLendingTrait<'borrow> {
    type Item;
}

pub struct ConstArgTarget<const N: usize>;

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
pub trait NoGenerics {}

// Lifetimes, type parameters, const parameters, defaults, and a direct `?Sized`
// bound are all normalized and the bound is moved into `where` position.
pub trait SimpleGenerics<'original, TypeParam: ?Sized, const COUNT: usize = 3> {}

// Lifetime bounds written directly in the generic parameter list are moved into
// `where` position and lifetime names are normalized by definition order.
pub trait LifetimeParamBounds<'short, 'long: 'short> {}

// Type and const parameters can interleave in definition order. Type names and
// const names use separate normalized sequences without changing that order.
pub trait InterleavedTypeConstParams<A, const B: usize, C, const D: usize>
where
    A: AsRef<[C; B]>,
    [C; D]: Clone,
{
}

// Supertrait bounds are normalized as `Self` predicates. This also exercises
// local public, re-exported local, private local, and `core` paths in the same
// bound list.
pub trait SupertraitBounds<T>:
    PublicTrait<Assoc = visible::PublicType>
    + ReexportedModuleTrait<Item = private::PrivateType>
    + private::PrivateTrait
    + core::fmt::Debug
where
    T: Clone,
{
}

// Associated item bound constraints on supertraits are lifted into projection
// predicates rooted at `Self`.
pub trait AssociatedSupertraitBounds<T>:
    IntoIterator<Item = T, IntoIter: ExactSizeIterator + DoubleEndedIterator>
where
    T: PublicTrait<Assoc = private::PrivateType>,
{
}

// Higher-ranked supertrait binders also apply to recursively normalized
// projection predicates for `<Self as PublicLendingTrait>::Item` and then for
// its own `Assoc` item.
pub trait HigherRankedSupertrait<T>:
    for<'borrow> PublicLendingTrait<
        'borrow,
        Item: PublicTrait<Assoc: AsRef<&'borrow visible::PublicType>>,
    >
where
    T: AsRef<visible::PublicType>,
{
}

// Explicit `Self` predicates in `where` clauses are normalized together with
// generic-parameter predicates.
pub trait ExplicitSelfWhere<T>
where
    Self: AsRef<T>,
    T: Clone,
{
}

// Concrete const expressions are preserved according to rustdoc's JSON string
// representation, just as they are for structs/enums/unions.
pub trait ConcreteConstExpr<const N: usize = { 1 + 2 }>
where
    [u8; 1 + 2]: Default,
    ConstArgTarget<{ 1 + 2 }>: Sized,
{
}
