use std::{borrow::Cow, collections::BTreeMap};

use rustdoc_types::{GenericParamDef, GenericParamDefKind};

// Avoid allocations for common amounts of generics: 8 of each kind.
const LIFETIME_NAMES: [&str; 8] = ["'a", "'b", "'c", "'d", "'e", "'f", "'g", "'h"];
const TYPE_NAMES: [&str; 8] = ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"];
const IMPL_TRAIT_TYPE_NAMES: [&str; 8] = ["IT1", "IT2", "IT3", "IT4", "IT5", "IT6", "IT7", "IT8"];
const CONST_NAMES: [&str; 8] = ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"];

/// Canonical names for generics visible in the current normalized signature.
///
/// Keys borrow generic parameter names from rustdoc data. Map values are
/// generated canonical spellings such as `T1`, `IT2`, `C1`, and `'a`; lookup
/// methods can also return borrowed special names such as `Self` and `'static`.
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
    pub(super) fn add_params(&mut self, params: &'a [GenericParamDef]) {
        for param in params {
            match param.kind {
                GenericParamDefKind::Lifetime { .. } => {
                    let index = self.lifetimes.len();
                    let normalized = LIFETIME_NAMES
                        .get(index)
                        .copied()
                        .map(Cow::Borrowed)
                        .unwrap_or_else(|| Cow::Owned(format!("'{}", letter_name(index))));
                    assert!(
                        self.lifetimes
                            .insert(lifetime_key(&param.name), normalized)
                            .is_none(),
                        "duplicate lifetime parameter `{}`",
                        param.name,
                    );
                }
                GenericParamDefKind::Type { is_synthetic, .. } => {
                    // Non-synthetic type parameters and parameter-position
                    // `impl Trait` share one normalized type-parameter counter.
                    let index = self.types.len();
                    let number = index + 1;
                    let predefined_names = if is_synthetic {
                        &IMPL_TRAIT_TYPE_NAMES
                    } else {
                        &TYPE_NAMES
                    };
                    let prefix = if is_synthetic { "IT" } else { "T" };
                    let normalized = predefined_names
                        .get(index)
                        .copied()
                        .map(Cow::Borrowed)
                        .unwrap_or_else(|| Cow::Owned(format!("{prefix}{number}")));
                    assert!(
                        self.types.insert(param.name.as_str(), normalized).is_none(),
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
                    assert!(
                        self.consts
                            .insert(param.name.as_str(), normalized)
                            .is_none(),
                        "duplicate const parameter `{}`",
                        param.name,
                    );
                }
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

    pub(super) fn const_name(&self, name: &str) -> Cow<'static, str> {
        self.consts
            .get(name)
            .cloned()
            .unwrap_or_else(|| unreachable!("unmapped const parameter `{name}`"))
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
    fn type_param_names_use_one_counter_with_static_prefix() {
        let params = (0..9)
            .flat_map(|index| {
                [
                    type_param(format!("T{index}"), false),
                    type_param(format!("impl Trait {index}"), true),
                ]
            })
            .collect::<Vec<_>>();
        let mut names = Names::default();
        names.add_params(&params);

        assert!(matches!(names.type_name("T0"), Cow::Borrowed("T1")));
        assert!(matches!(
            names.type_name("impl Trait 0"),
            Cow::Borrowed("IT2")
        ));
        assert!(matches!(names.type_name("T3"), Cow::Borrowed("T7")));
        assert!(matches!(
            names.type_name("impl Trait 3"),
            Cow::Borrowed("IT8")
        ));
        assert!(matches!(names.type_name("T4"), Cow::Owned(value) if value == "T9"));
        assert!(matches!(
            names.type_name("impl Trait 4"),
            Cow::Owned(value) if value == "IT10"
        ));
    }

    #[test]
    fn lifetime_and_const_names_use_static_prefix() {
        let params = (0..9)
            .map(|index| lifetime_param(format!("'lt{index}")))
            .chain((0..9).map(|index| const_param(format!("N{index}"))))
            .collect::<Vec<_>>();
        let mut names = Names::default();
        names.add_params(&params);

        assert!(matches!(names.lifetime("'lt0"), Cow::Borrowed("'a")));
        assert!(matches!(names.lifetime("'lt7"), Cow::Borrowed("'h")));
        assert!(matches!(names.lifetime("'lt8"), Cow::Owned(value) if value == "'i"));
        assert!(matches!(names.const_name("N0"), Cow::Borrowed("C1")));
        assert!(matches!(names.const_name("N7"), Cow::Borrowed("C8")));
        assert!(matches!(names.const_name("N8"), Cow::Owned(value) if value == "C9"));
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

    fn type_param(name: String, is_synthetic: bool) -> GenericParamDef {
        GenericParamDef {
            name,
            kind: GenericParamDefKind::Type {
                bounds: vec![],
                default: None,
                is_synthetic,
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
