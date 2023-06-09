pub trait Supertrait {}

pub trait Supertrait2 {}

pub trait BaseTrait : Supertrait2 + Supertrait {}
