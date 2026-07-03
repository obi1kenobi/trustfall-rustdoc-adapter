use rustdoc_types::{GenericParamDef, ItemEnum};

use crate::{PackageIndex, adapter::vertex::FunctionContext};

use super::{
    names::{Names, is_synthetic_type_param},
    parameter_impl_trait::{self, FnParameterImplTraits},
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

        // Parent and function generics use distinct placeholder families, but
        // they share lookup maps. Rust rejects shadowing across nested generic scopes
        // so there's no possibility of collisions.
        if let Some(item) = fn_ctx.parent {
            match &item.inner {
                ItemEnum::Trait(trait_) => {
                    for param in &trait_.generics.params {
                        names.add_parent_param(param);
                    }
                }
                ItemEnum::Impl(impl_) => {
                    for param in &impl_.generics.params {
                        names.add_parent_param(param);
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
            names.add_item_param(param);
        }
        let parameter_impl_traits =
            parameter_impl_trait::compute_for_function(crate_, &names, function_inner);

        Self {
            crate_,
            names,
            parameter_impl_traits,
        }
    }

    pub(super) fn with_higher_ranked_params<R>(
        &mut self,
        params: &'a [GenericParamDef],
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        // HRTB and function-pointer binders add a nested scope, but their
        // placeholder counters are monotonic across the whole signature,
        // to disambiguate in case another HRTB or function-pointer binder is introduced.
        // Keep the counters on this context and remove only the temporary lookup mappings
        // after the binder body has been formatted.
        let mut lifetime_keys = Vec::with_capacity(params.len());
        for param in params {
            lifetime_keys.push(self.names.add_higher_ranked_param(param));
        }
        let result = f(self);
        for key in lifetime_keys {
            self.names
                .lifetimes
                .remove(key)
                .expect("higher-ranked lifetime param was not in scope");
        }
        result
    }

    pub(super) fn crate_(&self) -> &'a PackageIndex<'a> {
        self.crate_
    }

    pub(super) fn names(&self) -> &Names<'a> {
        &self.names
    }

    pub(super) fn parameter_impl_traits(&self) -> &FnParameterImplTraits {
        &self.parameter_impl_traits
    }
}
