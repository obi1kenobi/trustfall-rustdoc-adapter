#![no_std]

#[no_mangle]
pub fn example_not_mangled() {}

#[export_name = "renamed"]
pub fn example_export_name() {}

#[no_mangle]
fn private_not_mangled() {}

#[export_name = "private_renamed"]
fn private_export_name() {}

#[no_mangle]
#[export_name = "renamed_3"]
fn export_name_not_mangled() {}

#[export_name = "renamed_4"]
#[no_mangle]
fn export_name_not_mangled_reversed() {}

pub fn mangled() {}
