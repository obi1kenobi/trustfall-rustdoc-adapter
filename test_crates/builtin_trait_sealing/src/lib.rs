use core::{
    fmt::Debug,
    hash::Hash,
    panic::{RefUnwindSafe, UnwindSafe},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Derived;

pub struct Dropped;

impl Drop for Dropped {
    fn drop(&mut self) {}
}

pub trait DerivedSupertraits:
    Clone + Copy + Debug + PartialEq + Eq + PartialOrd + Ord + Hash
{
}

pub trait AutoSupertraits: Send + Sync + Unpin + UnwindSafe + RefUnwindSafe {}

#[allow(drop_bounds)]
pub trait DropSupertrait: Drop {}

// `Sized` cannot be implemented explicitly, but it does not seal its subtraits.
pub trait SizedSupertrait: Sized {}

pub fn generic_bounds<T: Clone + Sized>(_value: T) {}

mod private {
    pub trait Sealed {}
}

pub mod shadow {
    pub trait Sized {}
    pub trait Clone: super::private::Sealed {}
}
