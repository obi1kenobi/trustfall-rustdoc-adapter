pub const FIRST: u32 = 1;

pub mod inner {
    pub const SECOND: i64 = 2;
}

pub struct Base;

impl Base {
    pub const ASSOCIATED: &'static str = "associated";
}
