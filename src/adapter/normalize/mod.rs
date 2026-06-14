//! Normalized signature rendering works from rustdoc's structured data, not
//! from Rust source text or already-rendered type strings. That keeps item IDs,
//! generic scopes, and path resolution available while formatting.
//!
//! The adapter layer decides which item component is being normalized and passes
//! the containing-item context here explicitly. Normalization is lazy:
//! we render the requested component after the query has traversed to a
//! normalized-signature vertex; we do not perform any kind of eager normalization.

mod context;
mod names;
mod parameter_impl_trait;
mod paths;
mod sort_key;
mod types;

use crate::PackageIndex;

use super::vertex::{FunctionContext, TypeSignatureComponent};

/// Produce a normalized type signature for a function param or return type.
pub(super) fn fn_param_or_return_type_signature<'a>(
    crate_: &'a PackageIndex<'a>,
    function: FunctionContext<'a>,
    component: TypeSignatureComponent,
    type_: Option<&'a rustdoc_types::Type>,
) -> String {
    let context = context::FnNormalizationContext::new(crate_, function);
    match (component, type_) {
        (TypeSignatureComponent::FunctionParameter(position), Some(type_)) => {
            types::format_parameter_type(&context, position, type_)
        }
        (TypeSignatureComponent::ReturnValue, Some(type_)) => types::format_type(&context, type_),
        (_, None) => "()".to_string(), // empty fn return values hit this branch
    }
}
