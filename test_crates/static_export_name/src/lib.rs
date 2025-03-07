#![no_std]

#[unsafe(no_mangle)]
pub static VAR1: i32 = 42;

#[unsafe(export_name = "EXTERNALLY_VISIBLE")]
pub static VAR2: i32 = 42;
