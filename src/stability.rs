use rustdoc_types::{Item, Visibility};

use crate::attributes::Attribute as ParsedAttribute;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PublicApiStabilityPolicy {
    Ignore,
    RustStandardLibrary,
}

impl PublicApiStabilityPolicy {
    pub(crate) fn item_is_unstable(self, _item: &Item) -> bool {
        // Stability info is not present in this rustdoc version.
        // Treat it as absent, so no item is marked unstable by rustdoc data.
        false
    }

    /// This is item-local eligibility, not path reachability.
    ///
    /// Eligible items can still be absent from stable public API,
    /// for example when they are public inside a private module.
    /// The local rules are:
    /// - public/default visibility (e.g. enum variants have default visibility), and
    /// - deprecated or not `#[doc(hidden)]`.
    ///
    /// Rustdoc JSON v57 does not expose item stability as structured data, so
    /// [`PublicApiStabilityPolicy::RustStandardLibrary`] behaves the same as
    /// [`PublicApiStabilityPolicy::Ignore`] on this branch.
    pub(crate) fn public_api_eligible(self, item: &Item) -> bool {
        let is_public = matches!(item.visibility, Visibility::Public | Visibility::Default);
        let allowed_by_doc_hidden =
            item.deprecation.is_some() || !item.attrs.iter().any(ParsedAttribute::is_doc_hidden);

        is_public && allowed_by_doc_hidden && !self.item_is_unstable(item)
    }

    pub(crate) fn effective_constness(self, function_item: &Item, raw_constness: bool) -> bool {
        assert!(
            matches!(function_item.inner, rustdoc_types::ItemEnum::Function(..)),
            "`function_item` was not a function: {function_item:?}"
        );

        // Stability info is not present in this rustdoc version.
        // Treat it as absent, so syntactic constness is the effective constness.
        raw_constness
    }
}
