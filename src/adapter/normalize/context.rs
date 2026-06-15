use rustdoc_types::{GenericParamDef, ItemEnum};

use crate::{PackageIndex, adapter::vertex::FunctionContext};

use super::{
    names::{Names, is_synthetic_type_param},
    parameter_impl_trait::{self, FnParameterImplTraits, ParameterImplTraitCursor},
};

/// Per-function state used while rendering one normalized signature.
///
/// A context is built for the function item that owns the parameter or return
/// value currently being rendered. It includes generics from the containing
/// trait or `impl` when there is one, but it is not shared across all methods in
/// an `impl` block: each function has its own rustdoc signature and its own
/// synthetic generic parameters.
#[derive(Clone)]
pub(super) struct FnNormalizationContext<'a> {
    crate_: &'a PackageIndex<'a>,
    names: Names<'a>,
    parameter_impl_traits: FnParameterImplTraits,
}

impl<'a> FnNormalizationContext<'a> {
    pub(super) fn new(crate_: &'a PackageIndex<'a>, fn_ctx: FunctionContext<'a>) -> Self {
        let mut names = Names::default();

        // Parent generics must be introduced before function generics so
        // normalized numbering follows the source-visible outer-to-inner scope.
        if let Some(item) = fn_ctx.parent {
            match &item.inner {
                ItemEnum::Trait(trait_) => {
                    for param in &trait_.generics.params {
                        names.add_param(param);
                    }
                }
                ItemEnum::Impl(impl_) => {
                    for param in &impl_.generics.params {
                        names.add_param(param);
                    }
                }
                _ => unreachable!("function parent was not a trait or impl: {item:?}"),
            }
        }

        let ItemEnum::Function(function_inner) = &fn_ctx.function.inner else {
            unreachable!(
                "normalized type signatures require a function item: {:?}",
                fn_ctx.function.inner,
            );
        };
        for param in &function_inner.generics.params {
            if is_synthetic_type_param(param) {
                // Parameter-position `impl Trait` params are named by
                // `parameter_impl_trait` because their placeholders are scoped to
                // their containing function parameter, not to the generic
                // parameter list as a whole.
                continue;
            }
            names.add_param(param);
        }
        let parameter_impl_traits =
            parameter_impl_trait::compute_for_function(crate_, function_inner, &names);

        Self {
            crate_,
            names,
            parameter_impl_traits,
        }
    }

    pub(super) fn with_params(&self, params: &'a [GenericParamDef]) -> Self {
        // HRTB and function-pointer binders add a nested scope. Clone the
        // context so outer generic mappings remain available while nested
        // parameters get fresh canonical names.
        // TODO: If this cloning becomes a bottleneck, we can probably design
        // a hierarchical name structure, since we don't expect deep levels of nesting.
        let mut value = self.clone();
        for param in params {
            value.names.add_param(param);
        }
        value
    }

    pub(super) fn crate_(&self) -> &'a PackageIndex<'a> {
        self.crate_
    }

    pub(super) fn names(&self) -> &Names<'a> {
        &self.names
    }

    pub(super) fn impl_trait_cursor_for_parameter(
        &self,
        position: std::num::NonZeroUsize,
    ) -> ParameterImplTraitCursor<'_> {
        self.parameter_impl_traits.cursor_for_parameter(position)
    }
}
