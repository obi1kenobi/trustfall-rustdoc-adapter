pub enum AllVariantTypes {
    First,                    // plain/unit variant
    Second(u32, String),      // tuple variant
    Third { x: i32, y: i32 }, // struct variant
    Fourth,                   // another plain variant
    Fifth(bool),              // another tuple variant
    Sixth { name: String },   // another struct variant
}

// Test explicit discriminant values
#[repr(u8)]
pub enum WithDiscriminants {
    A,      // 0
    B = 5,  // 5
    C,      // 6
    D = 10, // 10
    E,      // 11
}

// Test empty enum
pub enum Empty {}
