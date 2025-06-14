//! Ensuring that defaulted items overridden by the impl don't show up twice in queries.
//!
//! It is not allowed for a `const` and `method` to have the same name.
//! ```compile_fail
//! pub trait DefaultedType {
//!     const N: usize = 0;
//!
//!     fn N() {}
//! }
//! ```
//!
//! As of today, associated type defaults are unstable and don't compile.
//! ```compile_fail
//! pub trait DefaultedType {
//!     type Item = i64;
//! }
//! ```

pub trait Trait {
    const N: usize = 0;

    fn method() {}
}

struct Example;

impl Trait for Example {
    const N: usize = 1;

    fn method() {
        println!("hello world!");
    }
}
