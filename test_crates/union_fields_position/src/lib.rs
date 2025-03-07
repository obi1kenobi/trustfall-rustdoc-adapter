#![no_std]

#[repr(C)]
pub union SimpleUnion {
    pub first: i32,
    pub second: f32,
    pub third: u32,
}

#[repr(C)]
pub union UnionWithDifferentSizes {
    pub small: u8,
    pub medium: u32,
    pub large: u64,
}

#[repr(C)]
pub union UnionWithCompoundTypes {
    pub int_array: [i32; 4],
    pub float_array: [f32; 4],
}

// Empty union is not allowed in Rust
