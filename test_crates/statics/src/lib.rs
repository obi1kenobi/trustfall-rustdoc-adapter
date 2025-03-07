#![no_std]

pub static FIRST: u32 = 1;

pub static mut MUT: i64 = 0;

pub mod inner {
    pub static SECOND: i64 = 2;
}

unsafe extern "C" {
    pub safe static SAFE: i64;
    pub unsafe static UNSAFE: i64;
}
