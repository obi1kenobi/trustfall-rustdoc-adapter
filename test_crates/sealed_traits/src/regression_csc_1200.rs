//! Regression test for issue:
//! <https://github.com/obi1kenobi/cargo-semver-checks/issues/1200>
//!
//! When traits form a diamond with supertrait dependency,
//! that's not a cycle and the traits aren't sealed because of it.
//! All of the traits below are unsealed.

pub trait Base {}

pub trait Left: Base {}

pub trait Right: Base {}

pub trait Top: Left + Right {}
