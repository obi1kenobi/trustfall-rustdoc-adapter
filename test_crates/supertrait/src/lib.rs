pub trait Supertrait {}

pub trait Supertrait2 {}

pub trait MyTrait : Supertrait2 + Supertrait {}

// Ensure `std::fmt::Debug` is a supertrait here, verbatim.
// It's a load-bearing part of the test, since we want our query output names to be invariant
// to how they are written down in the source.
pub trait DebugPartialOrd : std::fmt::Debug + PartialOrd {}
