#![no_std]

#[unsafe(no_mangle)]
pub fn example_not_mangled() {}

#[unsafe(export_name = "renamed")]
pub fn example_export_name() {}

#[unsafe(no_mangle)]
fn private_not_mangled() {}

#[unsafe(export_name = "private_renamed")]
fn private_export_name() {}

#[expect(
    unused_attributes,
    reason = "test that `export_name` takes precedence over `no_mangle`"
)]
#[unsafe(no_mangle)]
#[unsafe(export_name = "renamed_3")]
fn export_name_not_mangled() {}

#[expect(
    unused_attributes,
    reason = "test that `export_name` takes precedence regardless of attribute order"
)]
#[unsafe(export_name = "renamed_4")]
#[unsafe(no_mangle)]
fn export_name_not_mangled_reversed() {}

pub fn mangled() {}
