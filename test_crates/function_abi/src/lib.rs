#![no_std]

pub extern "C-unwind" fn example_unwind() {}

pub extern "C" fn example_not_unwind() {}

pub fn rust_abi() {}

#[cfg(any(
    target_arch = "aarch64",
    target_arch = "arm",
    target_arch = "loongarch32",
    target_arch = "loongarch64",
    target_arch = "riscv32",
    target_arch = "riscv64",
    target_arch = "x86",
    target_arch = "x86_64",
))]
pub extern "efiapi" fn efiapi(_callback: extern "efiapi" fn()) {}
