#[no_mangle]
pub fn example_not_mangled() {}

#[export_name = "renamed"]
pub fn example_export_name() {}

#[no_mangle]
fn private_not_mangled() {}

#[export_name = "private_renamed"]
fn private_export_name() {}

pub fn mangled() {}
