#![no_std]

#[no_mangle]
pub static VAR1: i32 = 42;

#[export_name = "EXTERNALLY_VISIBLE"]
pub static VAR2: i32 = 42;

#[expect(
    unused_attributes,
    reason = "test that `export_name` takes precedence over `no_mangle`"
)]
#[no_mangle]
#[export_name = "EXTERNALLY_VISIBLE_3"]
pub static VAR3: i32 = 42;

#[expect(
    unused_attributes,
    reason = "test that `export_name` takes precedence regardless of attribute order"
)]
#[export_name = "EXTERNALLY_VISIBLE_4"]
#[no_mangle]
pub static VAR4: i32 = 42;
