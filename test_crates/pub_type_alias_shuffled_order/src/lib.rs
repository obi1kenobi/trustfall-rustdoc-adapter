//! This package exports the following:
//! - `ReversedGenericFoo<X, Y>`
//! - `ReversedLifetimeFoo<'x, 'y>`
//! - `ReversedConstFoo<const X: usize, const Y: usize>`
//!
//! However, these exported types are not considered re-exports of the underlying types!
//! Each of them has the generic parameter order flipped relative to the underlying type,
//! which results in a conceptually new type. This is not a re-export, and cannot be achieved
//! using a `pub use` statement.
//!
//! The `inner` module is private so the underlying types are not exported.

mod inner {
    pub struct GenericFoo<A, B> {
        _marker: std::marker::PhantomData<(A, B)>,
    }

    pub struct LifetimeFoo<'a, 'b> {
        _marker: std::marker::PhantomData<&'a &'b ()>,
    }

    pub struct ConstFoo<const A: usize, const B: usize> {
        _a: [i64; A],
        _b: [i64; B],
    }
}

// In the below type aliases, the generic parameters keep the same names
// but reverse their order relative to the underlying items.
// Keeping the names the same is intentional, if confusing:
// it makes sure that we compare the *meaning* of the generic type parameter
// rather than just its name relative to its underlying definition.

pub type ReversedGenericFoo<A, B> = crate::inner::GenericFoo<B, A>;

pub type ReversedLifetimeFoo<'a, 'b> = crate::inner::LifetimeFoo<'b, 'a>;

pub type ReversedConstFoo<const A: usize, const B: usize> = crate::inner::ConstFoo<B, A>;
