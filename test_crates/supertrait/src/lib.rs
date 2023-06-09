pub trait supertrait {}

pub trait supertrait2 {}

pub trait base_trait : supertrait2 + supertrait {}
