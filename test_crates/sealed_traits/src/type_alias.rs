struct Private;

#[doc(hidden)]
pub struct Hidden;

pub type IntRange = std::ops::Range<i64>;
type PrivateRange = std::ops::Range<i64>;

#[warn(private_interfaces)]
pub type AliasOfPrivate = Private;

#[warn(private_interfaces)]
pub type PrivateGeneric = std::ops::Range<Private>;

#[doc(hidden)]
pub type HiddenAlias = i64;

type PrivateAliasToHidden = Hidden;

pub type PubAliasToHidden = Hidden;

#[doc(hidden)]
pub type HiddenAliasToHidden = Hidden;

pub mod pub_type_alias {
    use super::IntRange;
    /// This trait is not sealed; the type alias is public.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::pub_type_alias::TakeTypeAlias for Witness {
    ///     fn method(&self, range: sealed_traits::type_alias::IntRange) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, range: IntRange);
    }

    /// This trait is not sealed; the type alias is public.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::pub_type_alias::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::IntRange {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> IntRange;
    }
}

pub mod private_type_alias {
    use super::PrivateRange;

    /// The type alias is private, but type aliases are just shorthand.
    /// The implementer can just use the explicit version of the type,
    /// which means this trait isn't sealed.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_type_alias::TakeTypeAlias for Witness {
    ///     fn method(&self, range: std::ops::Range<i64>) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, range: PrivateRange);
    }

    /// The type alias is private, but type aliases are just shorthand.
    /// The implementer can just use the explicit version of the type,
    /// which means this trait isn't sealed.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_type_alias::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> std::ops::Range<i64> {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> PrivateRange;
    }
}

pub mod private_type {
    use super::AliasOfPrivate;

    /// The type alias is pub, but the type it points to is private.
    /// The implementer cannot name the type and cannot use the alias.
    ///
    /// Proof:
    /// ```compile_fail
    /// fn witness(value: sealed_traits::type_alias::AliasOfPrivate) {}
    /// ```
    ///
    /// That means this trait is sealed.
    ///
    /// Proof:
    /// ```compile_fail
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_type::TakeTypeAlias for Witness {
    ///     fn method(&self, range: sealed_traits::type_alias::AliasOfPrivate) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, range: AliasOfPrivate);
    }

    /// By the same reasoning as above, this trait is sealed too.
    ///
    /// Proof:
    /// ```compile_fail
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_type::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::AliasOfPrivate {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> AliasOfPrivate;
    }
}

pub mod generic_private_type {
    use super::PrivateGeneric;

    /// The type alias is pub, but the type it points to is private due to the generic.
    /// The implementer cannot name the type and cannot use the alias.
    ///
    /// Proof:
    /// ```compile_fail
    /// fn witness(value: sealed_traits::type_alias::PrivateGeneric) {}
    /// ```
    ///
    /// That means this trait is sealed.
    ///
    /// Proof:
    /// ```compile_fail
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::generic_private_type::TakeTypeAlias for Witness {
    ///     fn method(&self, range: sealed_traits::type_alias::PrivateGeneric) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, range: PrivateGeneric);
    }

    /// By the same reasoning as above, this trait is sealed too.
    ///
    /// Proof:
    /// ```compile_fail
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::generic_private_type::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::PrivateGeneric {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> PrivateGeneric;
    }
}

pub mod hidden_alias {
    use super::HiddenAlias;

    /// The type alias is `doc(hidden)` but the underlying type is completely public API.
    /// The implementer can avoid the type alias and instead use the underlying type directly.
    ///
    /// That means this trait is not sealed at all.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::hidden_alias::TakeTypeAlias for Witness {
    ///     fn method(&self, value: i64) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, value: HiddenAlias);
    }

    /// By the same reasoning as above, this trait is unsealed too.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::hidden_alias::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> i64 {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> HiddenAlias;
    }
}

pub mod private_alias_to_hidden {
    use super::PrivateAliasToHidden;

    /// The type alias is private, and the type it points to is `#[doc(hidden)]`.
    /// The implementer cannot name the type alias,
    /// and cannot name the underlying type via public API.
    ///
    /// That means this trait is public API sealed; note the use of
    /// the `#[doc(hidden)] Hidden` type in the implementation.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_alias_to_hidden::TakeTypeAlias for Witness {
    ///     fn method(&self, value: sealed_traits::type_alias::Hidden) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, value: PrivateAliasToHidden);
    }

    /// By the same reasoning as above, this trait is public API sealed too.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::private_alias_to_hidden::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::Hidden {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> PrivateAliasToHidden;
    }
}

pub mod pub_alias_to_hidden {
    use super::PubAliasToHidden;

    /// The type alias is public API, but the type it points to is private due to the generic.
    /// The underlying type is not public API, but the type alias is.
    /// Downstream users can use the public type alias to implement this trait
    /// without using any non-public API.
    ///
    /// That means this trait is unsealed.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::pub_alias_to_hidden::TakeTypeAlias for Witness {
    ///     fn method(&self, range: sealed_traits::type_alias::PubAliasToHidden) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, range: PubAliasToHidden);
    }

    /// By the same reasoning as above, this trait is unsealed too.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::pub_alias_to_hidden::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::PubAliasToHidden {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> PubAliasToHidden;
    }
}

pub mod hidden_alias_to_hidden {
    use super::HiddenAliasToHidden;

    /// Both the type alias and its underlying type are `#[doc(hidden)]`.
    /// The implementer cannot name either via public API,
    /// so this trait is public API sealed.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::hidden_alias_to_hidden::TakeTypeAlias for Witness {
    ///     fn method(&self, value: sealed_traits::type_alias::HiddenAliasToHidden) {}
    /// }
    /// ```
    pub trait TakeTypeAlias {
        fn method(&self, value: HiddenAliasToHidden);
    }

    /// By the same reasoning as above, this trait is public API sealed too.
    ///
    /// Proof:
    /// ```
    /// struct Witness;
    ///
    /// impl sealed_traits::type_alias::hidden_alias_to_hidden::ReturnTypeAlias for Witness {
    ///     fn method(&self) -> sealed_traits::type_alias::HiddenAliasToHidden {
    ///         todo!()
    ///     }
    /// }
    /// ```
    pub trait ReturnTypeAlias {
        fn method(&self) -> HiddenAliasToHidden;
    }
}
