//! This package exports the following:
//! - `inner::DefaultConst`
//! - `inner::DefaultType`
//! - `inner::ConstOnly`
//! - `inner::TypeOnly`
//! - `OmittedConst`
//! - `OmittedType`
//! - `NonGenericConst`
//! - `NonGenericType`
//!
//! Typedefs that remove a generic parameter and rely on the underlying type's default value
//! change the semantics of the type, since that default cannot be overriden anymore.
//! The typedef cannot be treated as a re-export of the underlying type.

pub mod inner {
    pub struct DefaultConst<'a, T, const N: usize = 5> {
        _marker: std::marker::PhantomData<&'a [T; N]>,
    }

    pub struct DefaultType<'a, const N: usize, T = i64> {
        _marker: std::marker::PhantomData<&'a [T; N]>,
    }

    pub struct ConstOnly<const N: usize = 5> {
        _marker: std::marker::PhantomData<[i64; N]>,
    }

    pub struct TypeOnly<T = i64> {
        _marker: std::marker::PhantomData<T>,
    }
}

// The following types aren't semantically equivalent to the underlying types,
// so we consider them as separate typedefs rather than pure re-exports.
//
// They are not equivalent because they *don't allow any other value* for
// the generic parameter that contains a default. For example, one can't write:
// ```rust
// fn f(value: OmittedType<'a, N, T = ()>) {}
// ```
// whereas the equivalent is possible with the underlying type:
// ```rust
// fn f(value: DefaultType<'a, N, T = ()>) {}
// ```

pub type OmittedConst<'a, T> = crate::inner::DefaultConst<'a, T>;

pub type OmittedType<'a, const N: usize> = crate::inner::DefaultType<'a, N>;

// The following types also aren't semantically equivalent, for the same reason as above.
// They just show that typedefs can be non-generic even if the underlying type is generic.

pub type NonGenericConst = crate::inner::ConstOnly;

pub type NonGenericType = crate::inner::TypeOnly;
