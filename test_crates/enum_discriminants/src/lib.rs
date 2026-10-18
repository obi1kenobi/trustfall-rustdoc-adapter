#![no_std]

/// Some examples from <https://doc.rust-lang.org/reference/items/enumerations.html#implicit-discriminants>

#[repr(C)]
pub enum A {
    Zero,
    One = 1,
    Two = 1 + 1,
    Three,
    Four = 99,
    Five
}

#[repr(u8)]
pub enum FieldlessWithDiscrimants {
    First = 10,
    Tuple(),
    Second = 20,
    Struct{},
    Unit,
}

#[repr(u8)]
pub enum FieldlessWithExplicitDiscriminants {
    First = 10,
    Tuple() = 11,
    Second = 20,
    Struct{} = 21,
    Unit,
}

#[repr(C, i64)]
pub enum Fieldful {
    Unit,
    Tuple(bool),
    Struct{a: bool},
    Unit2 = 9,
    TupleExplicit(bool) = 10,
    StructExplicit{a: bool} = 11,
}

// No discriminants here: no repr + fields in the variants.
pub enum FieldfulNoRepr {
    Unit,
    Tuple(bool),
    Struct{a: bool},
}

pub enum EmptyStructVariant {
    Unit,
    Struct {},
}

pub enum EmptyTupleVariant {
    Unit,
    Tuple(),
}

pub enum FieldfulStructVariant {
    Unit,
    Struct { value: bool },
}

#[repr(i128)]
pub enum Pathological {
    Min = i128::MIN,
    MinPlusOne,
    MinPlusTwo,
    Max = i128::MAX,
}
