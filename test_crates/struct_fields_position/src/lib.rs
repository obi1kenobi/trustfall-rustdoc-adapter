#![no_std]

pub struct PlainStruct {
    pub first: i32,
    pub second: &'static str,
    pub third: bool,
}

pub struct TupleStruct(pub i32, pub &'static str, pub bool);

#[repr(C)]
pub struct ReprCStruct {
    pub a: i32,
    pub b: &'static str,
    pub c: bool,
}

#[repr(packed)]
pub struct ReprPackedStruct {
    pub x: i32,
    pub y: &'static str,
    pub z: bool,
}

#[repr(packed(2))]
pub struct ReprPackedWithAlignment {
    pub x: i32,
    pub y: bool,
    pub z: &'static str,
}

#[repr(C)]
pub struct ReprCTupleStruct(pub i32, pub &'static str, pub bool);

#[repr(transparent)]
pub struct ReprTransparentStruct {
    pub inner: i32,
}

pub struct EmptyStruct {}

pub struct UnitStruct;
