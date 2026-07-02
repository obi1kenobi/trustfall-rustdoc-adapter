#[doc(hidden)]
pub mod hidden_module {
    /// This trait is public-API-sealed because implementing it
    /// requires going outside the public API.
    pub trait HiddenSealed {}

    pub struct Token;

    #[deprecated]
    pub struct DeprecatedToken;
}

pub trait Unsealed {}

/// This trait is public-API-sealed since it isn't public API in the first place.
#[doc(hidden)]
pub trait DirectlyHiddenSealed {}

/// This trait is public-API-sealed since implementing it requires naming
/// its non-public-API supertrait.
pub trait HiddenSealedInherited: hidden_module::HiddenSealed {}

/// This trait is public-API-sealed transitively because of its supertrait.
pub trait TransitivelyHiddenSealed: HiddenSealedInherited {}

/// This trait is public-API-sealed, since `Self: hidden_module::HiddenSealed`
/// still requires that `Self` implement a public-API-sealed trait,
/// even though the public-API-sealed trait isn't *exactly* a supertrait.
pub trait HiddenSealedWithWhereSelfBound where Self: hidden_module::HiddenSealed {}

/// This trait is public-API-sealed because its method's argument type is doc-hidden,
/// so external implementers would have to name a non-public-API type to write the impl.
pub trait MethodHiddenSealed {
    fn method(&self, token: hidden_module::Token);
}

/// This trait is public-API-sealed because its method's return type is doc-hidden,
/// so external implementers would have to name a non-public-API type to write the impl.
pub trait MethodReturnHiddenSealed {
    fn method(&self) -> hidden_module::Token;
}

/// This trait is public-API-sealed because its required method is doc-hidden,
/// so external implementers would have to name a non-public-API method to write the impl.
pub trait HiddenMethodHiddenSealed {
    #[doc(hidden)]
    fn method(&self);
}

/// This trait is public-API-sealed since implementing its supertrait requires
/// naming a non-public-API type.
pub trait TransitivelyMethodHiddenSealed: MethodHiddenSealed {}

/// This trait is *not* public-API-sealed. Its method cannot be overridden within public API,
/// but implementing it is not required since the trait offers a default impl.
pub trait NotMethodHiddenSealedBecauseOfDefaultImpl {
    #[doc(hidden)]
    fn method(&self, _token: hidden_module::Token) -> i64 {
        0
    }
}

/// This trait is public-API-sealed because impls require
/// setting the non-public-API associated type.
//
// TODO: Add a test case with a default value on the associated type, when those become stable.
//       That would make the trait not sealed.
pub trait HiddenSealedAssocType {
    #[doc(hidden)]
    type T;
}

/// This trait is public-API-sealed because impls require
/// setting the non-public-API associated const.
pub trait HiddenSealedAssocConst {
    #[doc(hidden)]
    const N: usize;
}

/// This trait is public-API-sealed because impls require
/// setting the non-public-API associated const, which requires naming its type.
/// The const's type name is not public API, hence the trait is public-API-sealed.
pub trait HiddenSealedAssocConstType {
    const N: hidden_module::Token;
}

/// This trait is not sealed, since naming the doc-hidden const isn't necessary
/// as we can use its default value.
pub trait UnsealedDefaultAssocConst {
    #[doc(hidden)]
    const N: usize = 0;
}

/// This trait's method has a bound on a doc-hidden trait,
/// but the trait itself is *not* public-API-sealed!
/// Downstream implementations are possible without using non-public-API:
///
/// ```rust
/// struct Foo;
///
/// impl sealed_traits::doc_hidden::MethodWithHiddenBound for Foo {
///     fn method<IM>(&self) {}
/// }
/// ```
pub trait MethodWithHiddenBound {
    fn method<IM: hidden_module::HiddenSealed>(&self);
}

#[doc(hidden)]
pub mod blanket_impls {
    pub trait FullBlanket {}
    impl<T> FullBlanket for T {}

    pub trait RefBlanket {}
    impl<T> RefBlanket for &T {}

    pub trait ExternalSupertraitsBlanket {}
    impl<T: std::fmt::Debug + Clone> ExternalSupertraitsBlanket for T {}

    // In Rust this is syntax sugar for `impl<T: Clone>`, but let's make sure rustdoc thinks so too.
    pub trait BlanketWithWhereClause {}
    impl<T> BlanketWithWhereClause for T where T: Clone {}

    // The iterator trait is special because we don't manually inline it into rustdoc info.
    // See `MANUAL_TRAIT_ITEMS` inside `indexed_crate.rs` for more details.
    pub trait IteratorBlanket {}
    impl<T: Iterator> IteratorBlanket for T {}

    pub trait BlanketOverLocalUnsealedTrait {}
    impl<T: super::Unsealed> BlanketOverLocalUnsealedTrait for T {}

    pub trait BlanketOverSealedTrait {}
    impl<T: super::DirectlyHiddenSealed> BlanketOverSealedTrait for T {}

    pub trait BlanketOverSealedAndUnsealedTrait {}
    impl<T: super::Unsealed + super::DirectlyHiddenSealed> BlanketOverSealedAndUnsealedTrait for T {}

    // The blanket impl here is over everything,
    // because `FullBlanket` has a blanket impl for everything.
    pub trait TransitiveBlanket {}
    impl<T: FullBlanket> TransitiveBlanket for T {}

    pub trait BlanketOverArc {}
    impl<T> BlanketOverArc for std::sync::Arc<T> {}

    pub trait BlanketOverTuple {}
    impl<T> BlanketOverTuple for (T,) {}

    pub trait BlanketOverSlice {}
    impl<T> BlanketOverSlice for [T] {}

    pub trait BlanketOverArray {}
    impl<T> BlanketOverArray for [T; 1] {}

    pub trait BlanketOverPointer {}
    impl<T> BlanketOverPointer for *const T {}
}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// ```rust
/// struct Example;
///
/// impl sealed_traits::doc_hidden::BlanketUnsealed for Example {}
/// ```
pub trait BlanketUnsealed: blanket_impls::FullBlanket {}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::doc_hidden::RefBlanketUnsealed for &Example {}
/// ```
pub trait RefBlanketUnsealed: blanket_impls::RefBlanket {}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// Proof:
/// ```rust
/// #[derive(Debug, Clone)]
/// struct Example;
///
/// impl sealed_traits::doc_hidden::ExternalSupertraitsBlanketUnsealed for Example {}
/// ```
pub trait ExternalSupertraitsBlanketUnsealed: blanket_impls::ExternalSupertraitsBlanket {}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// Proof:
/// ```rust
/// #[derive(Clone)]
/// struct Example;
///
/// impl sealed_traits::doc_hidden::BlanketWithWhereClauseUnsealed for Example {}
/// ```
pub trait BlanketWithWhereClauseUnsealed: blanket_impls::BlanketWithWhereClause {}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// Proof:
/// ```rust
/// struct ExampleIter;
///
/// impl Iterator for ExampleIter {
///     type Item = ();
///
///     fn next(&mut self) -> Option<Self::Item> {
///         None
///     }
/// }
///
/// impl sealed_traits::doc_hidden::IteratorBlanketUnsealed for ExampleIter {}
/// ```
pub trait IteratorBlanketUnsealed: blanket_impls::IteratorBlanket {}

/// Not sealed due to blanket impl. No `doc(hidden)` items needed to impl it.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::doc_hidden::Unsealed for Example {}
///
/// impl sealed_traits::doc_hidden::BlanketOverLocalUnsealedTraitUnsealed for Example {}
/// ```
pub trait BlanketOverLocalUnsealedTraitUnsealed: blanket_impls::BlanketOverLocalUnsealedTrait {}

/// This one is public-API-sealed, since the blanket is over a public-API-sealed trait
/// which we cannot impl without touching non-public-API items:
/// - either we directly implement the public-API-sealed supertrait ourselves,
/// - or we implement the public-API-sealed trait for the supertrait's blanket impl.
pub trait BlanketOverSealedTraitSealed: blanket_impls::BlanketOverSealedTrait {}

/// This trait is public-API-sealed because the bound on the blanket impl
/// includes a trait we cannot impl without doc-hidden-API items. The proof is the same as above.
pub trait BlanketSealedOverMultiple: blanket_impls::BlanketOverSealedAndUnsealedTrait {}

/// This trait is not sealed, since its supertrait has a blanket impl whose bound
/// is always satisfied due to its own blanket impl.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::doc_hidden::TransitiveBlanketUnsealed for Example {}
/// ```
pub trait TransitiveBlanketUnsealed: blanket_impls::TransitiveBlanket {}

/// This trait is public-API-sealed.
/// - Its supertrait has a blanket impl over `Arc<T>`, but it isn't usable:
///   in order for a crate to implement a trait for a type, the crate needs to define
///   either the trait or the type. A downstream crate doesn't define either.
/// - That means the supertrait must be implemented directly, but it's `doc(hidden)`.
pub trait BlanketOverArcSealed: blanket_impls::BlanketOverArc {}

/// Public-API-sealed since tuples/slices/arrays/pointers are always considered foreign types.
pub trait BlanketOverTupleSealed: blanket_impls::BlanketOverTuple {}

/// Public-API-sealed since tuples/slices/arrays/pointers are always considered foreign types.
pub trait BlanketOverSliceSealed: blanket_impls::BlanketOverSlice {}

/// Public-API-sealed since tuples/slices/arrays/pointers are always considered foreign types.
pub trait BlanketOverArraySealed: blanket_impls::BlanketOverArray {}

/// Public-API-sealed since tuples/slices/arrays/pointers are always considered foreign types.
pub trait BlanketOverPointerSealed: blanket_impls::BlanketOverPointer {}

/// Not sealed due to being deprecated and therefore public API, regardless of `doc(hidden)`.
#[deprecated]
#[doc(hidden)]
pub trait DeprecatedHidden {}

/// Not sealed despite the supertrait, since the supertrait is deprecated and therefore public API.
pub trait UnsealedDueToDeprecatedSuper: DeprecatedHidden {}

/// Not sealed because the `doc(hidden)` associated type is deprecated and therefore public API.
pub trait DeprecatedAssocType {
    #[deprecated]
    #[doc(hidden)]
    type X;
}

/// Not sealed because the `doc(hidden)` associated const is deprecated and therefore public API.
pub trait DeprecatedAssocConst {
    #[deprecated]
    #[doc(hidden)]
    const N: usize;
}

/// Not sealed because the `doc(hidden)` method is deprecated and therefore public API.
pub trait DeprecatedMethod {
    #[deprecated]
    #[doc(hidden)]
    fn method(&self) {}
}

/// This trait is not sealed. Its method's argument type is doc-hidden but also deprecated,
/// which makes it public API.
pub trait MethodDeprecatedArgType {
    fn method(&self, token: hidden_module::DeprecatedToken);
}

/// A direct case of two traits declaring blankets on each other.
pub mod direct_cycle {
    pub mod hidden {
        #[doc(hidden)]
        pub trait DirectCycleSuper {}
    }

    pub trait DirectCycleSub: hidden::DirectCycleSuper {}

    impl<T: DirectCycleSub> hidden::DirectCycleSuper for T {}

    impl<T: hidden::DirectCycleSuper> DirectCycleSub for T {}
}
