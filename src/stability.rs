use rustdoc_types::{Item, Stability, StabilityLevel, Visibility};

use crate::attributes::Attribute as ParsedAttribute;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PublicApiStabilityPolicy {
    Ignore,
    RustStandardLibrary,
}

impl PublicApiStabilityPolicy {
    pub(crate) fn item_is_unstable(self, item: &Item) -> bool {
        // Rustdoc JSON emits inherited item-instability on associated items
        // when their instability is directly caused by an unstable trait or impl.
        //
        // We rely on that item-local `stability`, and we have a test to make sure
        // upstream doesn't stop emitting that data.
        self == Self::RustStandardLibrary && is_explicitly_unstable(item.stability.as_deref())
    }

    /// This is item-local eligibility, not path reachability.
    ///
    /// Eligible items can still be absent from stable public API,
    /// for example when they are public inside a private module.
    /// The local rules are:
    /// - public/default visibility (e.g. enum variants have default visibility),
    /// - deprecated or not `#[doc(hidden)]`, and
    /// - when in std library mode, not explicitly unstable.
    pub(crate) fn public_api_eligible(self, item: &Item) -> bool {
        let is_public = matches!(item.visibility, Visibility::Public | Visibility::Default);
        let allowed_by_doc_hidden =
            item.deprecation.is_some() || !item.attrs.iter().any(ParsedAttribute::is_doc_hidden);

        is_public && allowed_by_doc_hidden && !self.item_is_unstable(item)
    }

    pub(crate) fn effective_constness(self, function_item: &Item, raw_constness: bool) -> bool {
        // If the item isn't defined as `const` at all, it obviously isn't const.
        // If the item is defined as `const` and we aren't in the standard library mode
        // (where items' constness can be unstable), then it obviously is const.
        if !raw_constness || self == Self::Ignore {
            return raw_constness;
        }

        assert!(
            matches!(function_item.inner, rustdoc_types::ItemEnum::Function(..)),
            "`function_item` was not a function: {function_item:?}"
        );

        // Rustdoc JSON propagates inherited const-instability from
        // const-unstable traits and impls onto affected child function items.
        //
        // We have a test to make sure upstream doesn't stop emitting that data.
        !is_explicitly_unstable(function_item.const_stability.as_deref())
    }

    pub(crate) fn effective_function_has_body(self, function: &rustdoc_types::Function) -> bool {
        if !function.has_body || self == Self::Ignore {
            return function.has_body;
        }

        // Rustdoc JSON emits `default_unstable` only for unstable provided defaults.
        // In std mode, expose the stable guarantee by treating those defaults as absent.
        function.default_unstable.is_none()
    }

    pub(crate) fn effective_assoc_type_has_default(self, item: &Item) -> bool {
        let rustdoc_types::ItemEnum::AssocType {
            type_,
            default_unstable,
            ..
        } = &item.inner
        else {
            unreachable!("`item` was not an associated type: {item:?}");
        };

        type_.is_some() && (self == Self::Ignore || default_unstable.is_none())
    }

    pub(crate) fn effective_assoc_const_default(self, item: &Item) -> Option<&str> {
        let rustdoc_types::ItemEnum::AssocConst {
            value,
            default_unstable,
            ..
        } = &item.inner
        else {
            unreachable!("`item` was not an associated constant: {item:?}");
        };

        if self == Self::RustStandardLibrary && default_unstable.is_some() {
            None
        } else {
            value.as_deref()
        }
    }
}

fn is_explicitly_unstable(stability: Option<&Stability>) -> bool {
    matches!(
        stability.map(|stability| &stability.level),
        Some(StabilityLevel::Unstable)
    )
}
