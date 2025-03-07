#![no_std]

#[unsafe(no_mangle)]
pub fn example_not_mangled() {}

#[unsafe(export_name = "renamed")]
pub fn example_export_name() {}

#[unsafe(no_mangle)]
fn private_not_mangled() {}

#[unsafe(export_name = "private_renamed")]
fn private_export_name() {}

pub fn mangled() {}
