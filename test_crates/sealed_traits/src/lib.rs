mod private {
    pub trait Sealed {}

    pub struct Token;

    pub trait InternalMarker {}
}

/// This trait is sealed since nobody can implement its pub-in-priv supertrait.
pub trait DirectlyTraitSealed: private::Sealed {}

/// This trait is sealed since nobody can implement its supertrait.
pub trait TransitivelyTraitSealed: DirectlyTraitSealed {}

/// This trait is sealed, and happens to have more than one supertrait.
pub trait SealedTraitWithStdSupertrait: AsRef<()> + private::Sealed {}

trait PrivateSealed {}

/// This trait is sealed with a supertrait that is private, not pub-in-priv.
#[allow(private_bounds)]
pub trait SealedWithPrivateSupertrait: PrivateSealed {}

pub trait Unsealed {}

/// This trait is sealed because its argument type is pub-in-priv,
/// so external implementers cannot name it.
pub trait MethodSealed {
    fn method(&self, token: private::Token) -> i64;
}

/// This trait is sealed since nobody can implement its supertrait.
pub trait TransitivelyMethodSealed: MethodSealed {}

/// This trait is *not* sealed. Its method cannot be overridden,
/// but implementing it is not required since the trait offers a default impl.
pub trait NotMethodSealedBecauseOfDefaultImpl {
    fn method(&self, _token: private::Token) -> i64 {
        0
    }
}

/// This trait is *not* sealed. Its supertrait is also not sealed.
pub trait NotTransitivelySealed: NotMethodSealedBecauseOfDefaultImpl {}

/// This trait's method is generic-sealed, but the trait itself is *not* sealed!
/// Downstream users aren't able to call this method because
/// type-privacy will prevent inference of `IM` since the marker trait is pub-in-priv.
/// But downstream implementations are possible:
///
/// ```rust
/// struct Foo;
///
/// impl sealed_traits::TraitUnsealedButMethodGenericSealed for Foo {
///     fn method<IM>(&self) {}
/// }
/// ```
pub trait TraitUnsealedButMethodGenericSealed {
    fn method<IM: private::InternalMarker>(&self);
}

/// This trait is *not* sealed. Its method cannot be called, but can be overridden and
/// but implementing it is not required since the trait offers a default impl.
pub trait NotGenericSealedBecauseOfDefaultImpl {
    fn private_method<IM: private::InternalMarker>(&self) {}
}

/// This trait is *not* sealed. It merely depends on a trait from another crate.
pub trait IteratorExt: Iterator {}

pub mod shadow_builtins {
    /// This trait shadows the built-in (prelude) `Iterator` trait.
    /// However, it is supertrait-sealed.
    pub trait Iterator: super::private::Sealed {}

    /// This trait is also supertrait-sealed.
    pub trait ShadowedSubIterator: Iterator {}
}

pub mod generic_seal {
    pub trait Super {}

    mod private {
        pub trait Marker: super::Super {}
    }

    /// This trait is *not* generic-sealed, and neither is its method.
    ///
    /// While the `Marker` trait bound is pub-in-priv, it has a public supertrait `Super`.
    /// The `Super` bound can be used downstream to implement this trait:
    /// ```rust
    /// use sealed_traits::generic_seal::Super;
    /// use sealed_traits::generic_seal::NotGenericSealedBecauseOfPubSupertrait;
    ///
    /// struct Example;
    ///
    /// impl NotGenericSealedBecauseOfPubSupertrait for Example {
    ///     fn method<IM: Super>(&self) {}
    /// }
    /// ```
    ///
    /// An unbounded `IM` implementation is also allowed:
    /// ```rust
    /// use sealed_traits::generic_seal::NotGenericSealedBecauseOfPubSupertrait;
    ///
    /// struct Example;
    ///
    /// impl NotGenericSealedBecauseOfPubSupertrait for Example {
    ///     fn method<IM>(&self) {}
    /// }
    /// ```
    pub trait NotGenericSealedBecauseOfPubSupertrait {
        fn method<IM: private::Marker>(&self);
    }
}

/// Traits whose supertraits have blanket impls, where the blanket bounds
/// can be satisfied in a downstream crate, are not sealed.
mod blanket_impls {
    pub trait FullBlanket {}
    impl<T> FullBlanket for T {}

    // This trait is both crate-private and inside a private module.
    pub(crate) trait PrivateBlanket {}
    impl<T> PrivateBlanket for T {}

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
    impl<T: super::DirectlyTraitSealed> BlanketOverSealedTrait for T {}

    pub trait BlanketOverSealedAndUnsealedTrait {}
    impl<T: super::Unsealed + super::DirectlyTraitSealed> BlanketOverSealedAndUnsealedTrait for T {}

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

/// Not sealed due to blanket impl.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::BlanketUnsealed for Example {}
/// ```
#[allow(private_bounds)]
pub trait BlanketUnsealed: blanket_impls::FullBlanket + blanket_impls::PrivateBlanket {}

/// Not sealed due to blanket impl.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::RefBlanketUnsealed for &Example {}
/// ```
pub trait RefBlanketUnsealed: blanket_impls::RefBlanket {}

/// Not sealed due to blanket impl.
///
/// Proof:
/// ```rust
/// #[derive(Debug, Clone)]
/// struct Example;
///
/// impl sealed_traits::ExternalSupertraitsBlanketUnsealed for Example {}
/// ```
pub trait ExternalSupertraitsBlanketUnsealed: blanket_impls::ExternalSupertraitsBlanket {}

/// Not sealed due to blanket impl.
///
/// Proof:
/// ```rust
/// #[derive(Clone)]
/// struct Example;
///
/// impl sealed_traits::BlanketWithWhereClauseUnsealed for Example {}
/// ```
pub trait BlanketWithWhereClauseUnsealed: blanket_impls::BlanketWithWhereClause {}


/// Not sealed due to blanket impl.
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
/// impl sealed_traits::IteratorBlanketUnsealed for ExampleIter {}
/// ```
pub trait IteratorBlanketUnsealed: blanket_impls::IteratorBlanket {}

/// Not sealed due to blanket impl.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::Unsealed for Example {}
///
/// impl sealed_traits::BlanketOverLocalUnsealedTraitUnsealed for Example {}
/// ```
pub trait BlanketOverLocalUnsealedTraitUnsealed: blanket_impls::BlanketOverLocalUnsealedTrait {}

/// This one is sealed, since the blanket is over a sealed trait which we cannot impl.
///
/// Proof, in two parts:
///
/// We cannot implement the supertrait ourselves:
/// ```compile_fail
/// struct Example;
///
/// // The next line won't work, since the item is pub-in-priv
/// // so the trait is sealed and inaccessible.
/// impl sealed_traits::blanket_impls::BlanketOverSealedTrait for Example {}
///
/// impl sealed_traits::BlanketOverSealedTraitSealed for Example {}
/// ```
///
/// And we cannot implement the trait in the blanket bound:
/// ```compile_fail
/// struct Example;
///
/// // the next line won't work
/// impl sealed_traits::DirectlyTraitSealed for Example {}
///
/// impl sealed_traits::BlanketOverSealedTraitSealed for Example {}
/// ```
pub trait BlanketOverSealedTraitSealed: blanket_impls::BlanketOverSealedTrait {}

/// This trait is sealed because the bound on the blanket impl
/// includes a trait we cannot impl. The proof is the same as above.
pub trait BlanketSealedOverMultiple: blanket_impls::BlanketOverSealedAndUnsealedTrait {}

/// This trait is not sealed, since its supertrait has a blanket impl whose bound
/// is always satisfied due to its own blanket impl.
///
/// Proof:
/// ```rust
/// struct Example;
///
/// impl sealed_traits::TransitiveBlanketUnsealed for Example {}
/// ```
pub trait TransitiveBlanketUnsealed: blanket_impls::TransitiveBlanket {}

/// This trait is sealed.
/// - Its supertrait has a blanket impl over `Arc<T>`.
/// - In order for a crate to implement a trait for a type, the crate needs to define
///   either the trait or the type. A downstream crate doesn't define either.
///
/// Proof:
/// ```compile_fail
/// struct Example;
///
/// impl sealed_traits::BlanketOverArcSealed for std::sync::Arc<Example> {}
/// ```
pub trait BlanketOverArcSealed: blanket_impls::BlanketOverArc {}

/// Sealed since tuples/slices/arrays/pointers are always considered foreign types.
///
/// Proof:
/// ```compile_fail
/// struct Example;
///
/// impl sealed_traits::BlanketOverTupleSealed for (Example,) {}
/// ```
pub trait BlanketOverTupleSealed: blanket_impls::BlanketOverTuple {}

/// Sealed since tuples/slices/arrays/pointers are always considered foreign types.
///
/// Proof:
/// ```compile_fail
/// struct Example;
///
/// impl sealed_traits::BlanketOverSliceSealed for [Example] {}
/// ```
pub trait BlanketOverSliceSealed: blanket_impls::BlanketOverSlice {}

/// Sealed since tuples/slices/arrays/pointers are always considered foreign types.
///
/// Proof:
/// ```compile_fail
/// struct Example;
///
/// impl sealed_traits::BlanketOverArraySealed for [Example; 1] {}
/// ```
pub trait BlanketOverArraySealed: blanket_impls::BlanketOverArray {}

/// Sealed since tuples/slices/arrays/pointers are always considered foreign types.
///
/// Proof:
/// ```compile_fail
/// struct Example;
///
/// impl sealed_traits::BlanketOverPointerSealed for *const Example {}
/// ```
pub trait BlanketOverPointerSealed: blanket_impls::BlanketOverPointer {}
