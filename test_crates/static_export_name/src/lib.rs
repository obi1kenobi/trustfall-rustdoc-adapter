#![no_std]

#[unsafe(no_mangle)]
pub static VAR1: i32 = 42;

#[unsafe(export_name = "EXTERNALLY_VISIBLE")]
pub static VAR2: i32 = 42;

#[unsafe(no_mangle)]
#[unsafe(export_name = "EXTERNALLY_VISIBLE_3")]
pub static VAR3: i32 = 42;

#[unsafe(export_name = "EXTERNALLY_VISIBLE_4")]
#[unsafe(no_mangle)]
pub static VAR4: i32 = 42;
