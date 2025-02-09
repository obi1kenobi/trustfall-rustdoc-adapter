pub struct PlainStruct {
    pub first: i32,
    pub second: String,
    pub third: bool,
}

pub struct TupleStruct(pub i32, pub String, pub bool);

#[repr(C)]
pub struct ReprCStruct {
    pub a: i32,
    pub b: String,
    pub c: bool,
}

#[repr(packed)]
pub struct ReprPackedStruct {
    pub x: i32,
    pub y: String,
    pub z: bool,
}

#[repr(C)]
pub struct ReprCTupleStruct(pub i32, pub String, pub bool);

#[repr(transparent)]
pub struct ReprTransparentStruct {
    pub inner: i32,
}

pub struct EmptyStruct {}

pub struct UnitStruct;
