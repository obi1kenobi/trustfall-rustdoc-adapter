//! Shared rendering for function-pointer syntax.

use rustdoc_types::{Abi, FunctionHeader};

pub(super) fn format_fn_pointer_header(header: &FunctionHeader, output: &mut String) {
    if header.is_const {
        // As of Rust 1.96, function pointer types cannot use hypothetical
        // `const fn(...)` syntax, so there is no normalized signature
        // to produce for this header shape.
        unreachable!("found const function pointer header: {header:?}");
    }
    if header.is_async {
        // As of Rust 1.96, function pointer types cannot use hypothetical
        // `async fn(...)` syntax, so there is no normalized signature
        // to produce for this header shape.
        unreachable!("found async function pointer header: {header:?}");
    }
    if header.is_unsafe {
        output.push_str("unsafe ");
    }

    match &header.abi {
        Abi::Rust => {}
        Abi::C { unwind } => push_abi(output, "C", *unwind),
        Abi::Cdecl { unwind } => push_abi(output, "cdecl", *unwind),
        Abi::Stdcall { unwind } => push_abi(output, "stdcall", *unwind),
        Abi::Fastcall { unwind } => push_abi(output, "fastcall", *unwind),
        Abi::Aapcs { unwind } => push_abi(output, "aapcs", *unwind),
        Abi::Win64 { unwind } => push_abi(output, "win64", *unwind),
        Abi::SysV64 { unwind } => push_abi(output, "sysv64", *unwind),
        Abi::System { unwind } => push_abi(output, "system", *unwind),
        Abi::Other(other) => {
            output.push_str("extern \"");
            output.push_str(other);
            output.push_str("\" ");
        }
    }
}

fn push_abi(output: &mut String, name: &str, unwind: bool) {
    output.push_str("extern \"");
    output.push_str(name);
    if unwind {
        output.push_str("-unwind");
    }
    output.push_str("\" ");
}
