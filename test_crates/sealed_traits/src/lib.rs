mod private {
    pub trait Sealed {}

    pub struct Token;

    pub trait InternalMarker {}
}

/// This trait is sealed since nobody can implement its pub-in-priv supertrait.
pub trait DirectlyTraitSealed: private::Sealed {}

/// This trait is sealed since nobody can implement its supertrait.
pub trait TransitivelyTraitSealed: DirectlyTraitSealed {}

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

/// This trait's method is generic-sealed: downstream implementors are not able
/// to write the trait bound since the internal marker trait is pub-in-priv.
pub trait GenericSealed {
    fn method<IM: private::InternalMarker>(&self);
}

/// This trait is *not* sealed. Its method cannot be overridden,
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

    /// This trait is *not* generic-sealed.
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
    pub trait NotGenericSealedBecauseOfPubSupertrait {
        fn method<IM: private::Marker>(&self);
    }
}
