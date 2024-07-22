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

#[repr(i64)]
pub enum Fieldful {
    Unit,
    Tuple(bool),
    Struct{a: bool},
    Unit2 = 9
}

