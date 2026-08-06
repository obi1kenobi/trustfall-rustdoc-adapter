use std::ops::Deref;

// Example adapted from <https://rust-lang.github.io/rfcs/1598-generic_associated_types.html>

pub trait LifetimeGenericTrait {
    type Item<'a>;
}

pub trait TypeGenericTrait {
    type Item<T>: Deref<Target = T>;
}

pub trait TypeLifetimeGenericTrait {
    type Item<'a, T>: Deref<Target = T>;
}

pub trait ConstTrait<const N: usize> {}

pub trait ConstGenericTrait {
    type ConstTrait<const N: usize>: ConstTrait<N>;
}
