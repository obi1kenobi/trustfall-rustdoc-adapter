use std::{borrow::Cow, collections::BTreeMap};

use rustdoc_types::{GenericParamDef, GenericParamDefKind};

// Avoid allocations for common amounts of generics: 8 of each kind.
const LIFETIME_NAMES: [&str; 8] = ["'a", "'b", "'c", "'d", "'e", "'f", "'g", "'h"];
const TYPE_NAMES: [&str; 8] = ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"];
const CONST_NAMES: [&str; 8] = ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"];
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
    lifetimes: BTreeMap<&'a str, Cow<'static, str>>,
    types: BTreeMap<&'a str, Cow<'static, str>>,
    consts: BTreeMap<&'a str, Cow<'static, str>>,
}

impl<'a> Names<'a> {
    /// Adds one source-visible generic parameter to this normalized name scope.
    ///
    /// Rustdoc also creates synthetic function-level type params for
    /// parameter-position `impl Trait`. Those params are intentionally rejected
    /// here because their placeholders are scoped to the containing function
    /// parameter and are assigned by `parameter_impl_trait` instead.
    pub(super) fn add_param(&mut self, param: &'a GenericParamDef) {
        match param.kind {
            GenericParamDefKind::Lifetime { .. } => {
                let index = self.lifetimes.len();
                let normalized = LIFETIME_NAMES
                    .get(index)
                    .copied()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(format!("'{}", letter_name(index))));
                let existing = self.lifetimes.insert(lifetime_key(&param.name), normalized);
                assert!(
                    existing.is_none(),
                    "duplicate lifetime parameter `{}`",
                    param.name,
                );
            }
            GenericParamDefKind::Type { is_synthetic, .. } => {
                assert!(
                    !is_synthetic,
                    "parameter-position impl Trait params must be handled by parameter_impl_trait",
                );
                let index = self.types.len();
                let number = index + 1;
                let normalized = TYPE_NAMES
                    .get(index)
                    .copied()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(format!("T{number}")));
                let existing = self.types.insert(param.name.as_str(), normalized);
                assert!(
                    existing.is_none(),
                    "duplicate type parameter `{}`",
                    param.name,
                );
            }
            GenericParamDefKind::Const { .. } => {
                let index = self.consts.len();
                let number = index + 1;
                let normalized = CONST_NAMES
                    .get(index)
                    .copied()
                    .map(Cow::Borrowed)
                    .unwrap_or_else(|| Cow::Owned(format!("C{number}")));
                let existing = self.consts.insert(param.name.as_str(), normalized);
                assert!(
                    existing.is_none(),
                    "duplicate const parameter `{}`",
                    param.name,
                );
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

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use rustdoc_types::{GenericParamDef, GenericParamDefKind, Type};

    use super::Names;

    #[test]
    fn type_param_names_use_static_prefix() {
        let params = (0..9)
            .map(|index| type_param(format!("InputT{index}")))
            .collect::<Vec<_>>();
        let mut names = Names::default();
        for param in &params {
            names.add_param(param);
        }

        assert!(matches!(names.type_name("InputT0"), Cow::Borrowed("T1")));
        assert!(matches!(names.type_name("InputT7"), Cow::Borrowed("T8")));
        assert!(matches!(names.type_name("InputT8"), Cow::Owned(value) if value == "T9"));
    }

    #[test]
    fn lifetime_and_const_names_use_static_prefix() {
        let params = (0..9)
            .map(|index| lifetime_param(format!("'lt{index}")))
            .chain((0..9).map(|index| const_param(format!("N{index}"))))
            .collect::<Vec<_>>();
        let mut names = Names::default();
        for param in &params {
            names.add_param(param);
        }

        assert!(matches!(names.lifetime("'lt0"), Cow::Borrowed("'a")));
        assert!(matches!(names.lifetime("'lt7"), Cow::Borrowed("'h")));
        assert!(matches!(names.lifetime("'lt8"), Cow::Owned(value) if value == "'i"));
        assert!(matches!(
            names.type_or_const_name("N0"),
            Cow::Borrowed("C1")
        ));
        assert!(matches!(
            names.type_or_const_name("N7"),
            Cow::Borrowed("C8")
        ));
        assert!(matches!(names.type_or_const_name("N8"), Cow::Owned(value) if value == "C9"));
        assert!(matches!(names.const_expr("N0"), Cow::Borrowed("C1")));
        assert!(matches!(names.const_expr("N8"), Cow::Owned(value) if value == "C9"));

        let expression = String::from("N0 + 1");
        assert!(matches!(
            names.const_expr(&expression),
            Cow::Borrowed(value) if value == expression
        ));
    }

    fn lifetime_param(name: String) -> GenericParamDef {
        GenericParamDef {
            name,
            kind: GenericParamDefKind::Lifetime { outlives: vec![] },
        }
    }

    fn type_param(name: String) -> GenericParamDef {
        GenericParamDef {
            name,
            kind: GenericParamDefKind::Type {
                bounds: vec![],
                default: None,
                is_synthetic: false,
            },
        }
    }

    fn const_param(name: String) -> GenericParamDef {
        GenericParamDef {
            name,
            kind: GenericParamDefKind::Const {
                type_: Type::Primitive("usize".into()),
                default: None,
            },
        }
    }
}
