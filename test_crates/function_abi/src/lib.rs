pub extern "C-unwind" fn example_unwind() {}

pub extern "C" fn example_not_unwind() {}

pub fn rust_abi() {}
