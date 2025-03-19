#![no_std]

#[no_mangle]
pub static VAR1: i32 = 42;

#[export_name = "EXTERNALLY_VISIBLE"]
pub static VAR2: i32 = 42;

#[no_mangle]
#[export_name = "EXTERNALLY_VISIBLE_3"]
pub static VAR3: i32 = 42;

#[export_name = "EXTERNALLY_VISIBLE_4"]
#[no_mangle]
pub static VAR4: i32 = 42;
