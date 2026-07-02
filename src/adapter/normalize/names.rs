use std::{borrow::Cow, collections::BTreeMap};

use rustdoc_types::{GenericParamDef, GenericParamDefKind};

// Avoid allocations for common amounts of generics: 8 of each kind.
const ITEM_LIFETIME_NAMES: [&str; 8] = ["'a", "'b", "'c", "'d", "'e", "'f", "'g", "'h"];
const ITEM_TYPE_NAMES: [&str; 8] = ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"];
const ITEM_CONST_NAMES: [&str; 8] = ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"];
const PARENT_LIFETIME_NAMES: [&str; 8] = ["'o1", "'o2", "'o3", "'o4", "'o5", "'o6", "'o7", "'o8"];
const PARENT_TYPE_NAMES: [&str; 8] = ["TO1", "TO2", "TO3", "TO4", "TO5", "TO6", "TO7", "TO8"];
const PARENT_CONST_NAMES: [&str; 8] = ["CO1", "CO2", "CO3", "CO4", "CO5", "CO6", "CO7", "CO8"];
const HIGHER_RANKED_LIFETIME_NAMES: [&str; 8] =
    ["'b1", "'b2", "'b3", "'b4", "'b5", "'b6", "'b7", "'b8"];
const FIRST_IMPL_TRAIT_PLACEHOLDER_BY_PARAMETER: [&str; 8] = [
    "IT1_1", "IT2_1", "IT3_1", "IT4_1", "IT5_1", "IT6_1", "IT7_1", "IT8_1",
];

/// Canonical names for generics visible in the current normalized signature.
///
/// Keys for source-named generics borrow generic parameter names from rustdoc
/// data. Map values are generated canonical spellings such as `T1`, `C1`, and
/// `'a`; lookup methods can also return borrowed special names such as `Self`
/// and `'static`.
///
/// A missing lookup for a named generic is a bug in scope construction, not a
/// cue to preserve the source spelling. Arbitrary const expressions are the
/// exception: rustdoc gives them as strings, and many are not bare const
/// parameter names.
#[derive(Clone, Debug, Default)]
pub(super) struct Names<'a> {
    pub(super) lifetimes: BTreeMap<&'a str, Cow<'static, str>>,
    types: BTreeMap<&'a str, Cow<'static, str>>,
    consts: BTreeMap<&'a str, Cow<'static, str>>,
    next_item_lifetime: usize,
    next_item_type: usize,
    next_item_const: usize,
    next_parent_lifetime: usize,
    next_parent_type: usize,
    next_parent_const: usize,
    next_higher_ranked_lifetime: usize,
}

impl<'a> Names<'a> {
    /// Adds one source-visible generic parameter from a containing `trait` or `impl`.
    pub(super) fn add_parent_param(&mut self, param: &'a GenericParamDef) {
        match &param.kind {
            GenericParamDefKind::Lifetime { .. } => {
                let name =
                    numbered_name(&mut self.next_parent_lifetime, &PARENT_LIFETIME_NAMES, "'o");
                self.insert_lifetime_param(param, name);
            }
            GenericParamDefKind::Type { is_synthetic, .. } => {
                assert!(
                    !is_synthetic,
                    "parameter-position impl Trait params must be handled by parameter_impl_trait",
                );
                let name = numbered_name(&mut self.next_parent_type, &PARENT_TYPE_NAMES, "TO");
                self.insert_type_param(param, name);
            }
            GenericParamDefKind::Const { .. } => {
                let name = numbered_name(&mut self.next_parent_const, &PARENT_CONST_NAMES, "CO");
                self.insert_const_param(param, name);
            }
        }
    }

    /// Adds one source-visible generic parameter from the item being normalized.
    ///
    /// Rustdoc also creates synthetic function-level type params for
    /// parameter-position `impl Trait`. Those params are intentionally rejected
    /// here because their placeholders are scoped to the containing function
    /// parameter and are assigned by `parameter_impl_trait` instead.
    pub(super) fn add_item_param(&mut self, param: &'a GenericParamDef) {
        match &param.kind {
            GenericParamDefKind::Lifetime { .. } => {
                let index = self.next_item_lifetime;
                self.next_item_lifetime += 1;
                let name = ITEM_LIFETIME_NAMES
                    .get(index)
                    .copied()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(format!("'{}", letter_name(index))));
                self.insert_lifetime_param(param, name);
            }
            GenericParamDefKind::Type { is_synthetic, .. } => {
                assert!(
                    !is_synthetic,
                    "parameter-position impl Trait params must be handled by parameter_impl_trait",
                );
                let name = numbered_name(&mut self.next_item_type, &ITEM_TYPE_NAMES, "T");
                self.insert_type_param(param, name);
            }
            GenericParamDefKind::Const { .. } => {
                let name = numbered_name(&mut self.next_item_const, &ITEM_CONST_NAMES, "C");
                self.insert_const_param(param, name);
            }
        }
    }

    /// Adds one lifetime parameter from a higher-ranked binder.
    pub(super) fn add_higher_ranked_param(&mut self, param: &'a GenericParamDef) -> &'a str {
        match &param.kind {
            GenericParamDefKind::Lifetime { .. } => {
                let name = numbered_name(
                    &mut self.next_higher_ranked_lifetime,
                    &HIGHER_RANKED_LIFETIME_NAMES,
                    "'b",
                );
                self.insert_lifetime_param(param, name)
            }
            GenericParamDefKind::Type { .. } => {
                unreachable!("found type generic param definition in HRTB position: {param:?}")
            }
            GenericParamDefKind::Const { .. } => {
                unreachable!("found const generic param definition in HRTB position: {param:?}")
            }
        }
    }

    pub(super) fn lifetime(&self, name: &str) -> Cow<'static, str> {
        let key = lifetime_key(name);
        match key {
            "static" => Cow::Borrowed("'static"),
            "_" => Cow::Borrowed("'_"),
            _ => self
                .lifetimes
                .get(key)
                .cloned()
                .unwrap_or_else(|| unreachable!("unmapped lifetime parameter `{name}`")),
        }
    }

    pub(super) fn type_name(&self, name: &str) -> Cow<'static, str> {
        if name == "Self" {
            Cow::Borrowed("Self")
        } else {
            self.types
                .get(name)
                .cloned()
                .unwrap_or_else(|| unreachable!("unmapped type parameter `{name}`"))
        }
    }

    pub(super) fn type_or_const_name(&self, name: &str) -> Cow<'static, str> {
        self.types
            .get(name)
            .or_else(|| self.consts.get(name))
            .cloned()
            .unwrap_or_else(|| unreachable!("unmapped type or const parameter `{name}`"))
    }

    pub(super) fn const_expr<'b>(&self, expr: &'b str) -> Cow<'b, str> {
        match self.consts.get(expr) {
            Some(Cow::Borrowed(name)) => Cow::Borrowed(*name),
            Some(Cow::Owned(name)) => Cow::Owned(name.clone()),
            None => Cow::Borrowed(expr),
        }
    }

    fn insert_lifetime_param(
        &mut self,
        param: &'a GenericParamDef,
        normalized: Cow<'static, str>,
    ) -> &'a str {
        let key = lifetime_key(&param.name);
        let existing = self.lifetimes.insert(key, normalized);
        assert!(
            existing.is_none(),
            "duplicate lifetime parameter `{}`",
            param.name,
        );
        key
    }

    fn insert_type_param(&mut self, param: &'a GenericParamDef, normalized: Cow<'static, str>) {
        let key = param.name.as_str();
        let existing = self.types.insert(key, normalized);
        assert!(
            existing.is_none(),
            "duplicate type parameter `{}`",
            param.name,
        );
    }

    fn insert_const_param(&mut self, param: &'a GenericParamDef, normalized: Cow<'static, str>) {
        let key = param.name.as_str();
        let existing = self.consts.insert(key, normalized);
        assert!(
            existing.is_none(),
            "duplicate const parameter `{}`",
            param.name,
        );
    }
}

pub(super) fn parameter_impl_trait_placeholder(
    parameter_position: usize,
    occurrence_position: usize,
) -> Cow<'static, str> {
    assert!(
        parameter_position > 0,
        "function parameter positions are 1-based"
    );
    assert!(
        occurrence_position > 0,
        "impl Trait positions within a parameter are 1-based"
    );

    if occurrence_position == 1 {
        if let Some(name) = FIRST_IMPL_TRAIT_PLACEHOLDER_BY_PARAMETER.get(parameter_position - 1) {
            return Cow::Borrowed(name);
        }
    }

    Cow::Owned(format!("IT{parameter_position}_{occurrence_position}"))
}

pub(super) fn is_synthetic_type_param(param: &GenericParamDef) -> bool {
    matches!(
        param.kind,
        GenericParamDefKind::Type {
            is_synthetic: true,
            ..
        }
    )
}

fn lifetime_key(name: &str) -> &str {
    name.strip_prefix('\'').unwrap_or(name)
}

fn numbered_name(
    counter: &mut usize,
    precomputed: &'static [&'static str],
    fallback_prefix: &'static str,
) -> Cow<'static, str> {
    let index = *counter;
    *counter += 1;
    precomputed
        .get(index)
        .copied()
        .map(Cow::Borrowed)
        .unwrap_or_else(|| Cow::Owned(format!("{fallback_prefix}{}", index + 1)))
}

fn letter_name(mut index: usize) -> String {
    let mut output = String::new();

    loop {
        let offset = (index % 26) as u8;
        output.insert(0, char::from(b'a' + offset));
        if index < 26 {
            break;
        }
        index = (index / 26) - 1;
    }

    output
}
