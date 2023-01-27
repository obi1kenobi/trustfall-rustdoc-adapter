//! This package exports the following:
//! - `inner::GenericFoo<A, B>`
//! - `ChangedFoo<const A: usize, const B: usize>`
//!
//! However, the `pub type ChangedFoo` is not a re-export of its underlying type,
//! since it changes the meaning of the generic parameters relative to the underlying type.
//! This is not consistent with a re-export, and cannot be achieved using a `pub use` statement.

pub mod inner {
    pub struct GenericFoo<A, B> {
        _marker: std::marker::PhantomData<(A, B)>,
    }
}

// The below type aliases include the same generic parameters as the underlying types,
// but do not use them in a way that is equivalent to the underlying type.
//
// Keeping the names the same is intentional, if confusing:
// it makes sure that we compare the *meaning* of the generic type parameter
// rather than just its name relative to its underlying definition.

pub type ChangedFoo<A, B> = crate::inner::GenericFoo<(A, B), i64>;

// TODO: when generic const expressions become stable, include a test case like:
// ```rust
// pub struct ConstBar<const A: usize, const B: usize> {
//     _a: [i64; A],
//     _b: [i64; B],
// }
//
// pub type ChangedBar<const A: usize, const B: usize> = crate::ConstBar<A + B, 3>;
// ```
