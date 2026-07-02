#![no_std]

#[non_exhaustive]
pub enum NonExhaustiveEnum {
    First,
}

#[non_exhaustive]
pub struct NonExhaustive {
    pub x: i64,
}

pub enum MyEnum {
    #[non_exhaustive]
    NonExhaustiveVariant(i64),
}
