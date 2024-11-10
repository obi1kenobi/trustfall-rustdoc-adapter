//! Notes on test completeness, as of Rust 1.81:
//! - Functions annotated with `#[proc_macro]` must be private, or else it's a compile error.
//! - All proc macros must be defined at the root of the crate, or else it's a compile error.
//! - Proc macro crates cannot export any other items other than proc macros. Not even pub modules
//!   are allowed!

extern crate proc_macro;
use proc_macro::TokenStream;

/// Example from
/// <https://doc.rust-lang.org/reference/procedural-macros.html#function-like-procedural-macros>
///
/// Use:
/// ```rust
/// extern crate proc_macros;
/// use proc_macros::make_answer;
///
/// make_answer!();
///
/// fn main() {
///     println!("{}", answer());
/// }
/// ```
#[proc_macro]
pub fn make_answer(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}

/// Example from <https://doc.rust-lang.org/reference/procedural-macros.html#attribute-macros>
///
/// Use:
/// ```rust
/// #[proc_macros::return_as_is]
/// struct Unit;
/// ```
#[proc_macro_attribute]
pub fn return_as_is(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

/// Example from <https://doc.rust-lang.org/reference/procedural-macros.html#derive-macros>
///
/// Use:
/// ```rust
/// extern crate proc_macros;
/// use proc_macros::AnswerFn;
///
/// #[derive(AnswerFn)]
/// struct Struct;
///
/// fn main() {
///     assert_eq!(42, answer());
/// }
/// ```
#[proc_macro_derive(AnswerFn)]
pub fn derive_answer_fn(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}

/// Example tweaked from
/// <https://doc.rust-lang.org/reference/procedural-macros.html#derive-macro-helper-attributes>
///
/// Use:
/// ```rust
/// #[derive(proc_macros::HelperAttr)]
/// struct Struct {
///     #[helper] field: (),
///     #[second] other: (),
/// }
/// ```
#[proc_macro_derive(HelperAttr, attributes(helper, second))]
pub fn derive_helper_attr(_item: TokenStream) -> TokenStream {
    TokenStream::new()
}

/// Usable but hidden:
/// ```rust
/// extern crate proc_macros;
///
/// proc_macros::hidden!();
///
/// fn main() {
///     println!("{}", answer());
/// }
/// ```
#[doc(hidden)]
#[proc_macro]
pub fn hidden(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
