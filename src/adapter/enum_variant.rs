use rustdoc_types::{Item, ItemEnum, Variant};
use std::fmt;
use std::num::ParseIntError;
use std::sync::Arc;
use std::{str::FromStr, sync::OnceLock};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub(super) struct EnumVariant<'a> {
    item: &'a Item,
    discriminants: Arc<LazyDiscriminants<'a>>,
    index: usize,
}

#[non_exhaustive]
#[derive(Debug, Clone)]
pub(super) struct LazyDiscriminants<'a> {
    variants: Vec<&'a Variant>,
    discriminants: OnceLock<Vec<String>>,
}

impl<'a> LazyDiscriminants<'a> {
    pub(super) fn new(variants: Vec<&'a Variant>) -> Self {
        Self {
            variants,
            discriminants: OnceLock::new(),
        }
    }

    pub(super) fn get_discriminants(&self) -> &Vec<String> {
        self.discriminants
            .get_or_init(|| assign_discriminants(&self.variants))
    }
}

impl<'a> EnumVariant<'a> {
    pub(super) fn new(
        item: &'a Item,
        discriminants: Arc<LazyDiscriminants<'a>>,
        index: usize,
    ) -> Self {
        Self {
            item,
            discriminants,
            index,
        }
    }

    pub(super) fn variant(&self) -> Option<&'a Variant> {
        match &self.item.inner {
            ItemEnum::Variant(v) => Some(v),
            _ => None,
        }
    }

    pub(super) fn discriminant(&'a self) -> &'a String {
        self.discriminants
            .get_discriminants()
            .get(self.index)
            .unwrap()
    }
}

enum DiscriminantValue {
    I64(i64),
    U64(u64),
    I128(i128),
    U128(u128),
}

impl DiscriminantValue {
    pub fn max(&self) -> bool {
        matches!(self, DiscriminantValue::U128(u128::MAX))
    }

    pub fn increment(&self) -> DiscriminantValue {
        match self {
            DiscriminantValue::I64(i) => {
                match i.checked_add_unsigned(1) {
                    // No overflow
                    Some(i) => i.into(),
                    // Overflow, number will fit in a u64
                    None => DiscriminantValue::from(u64::try_from(*i).unwrap()).increment(),
                }
            }
            DiscriminantValue::U64(i) => {
                match i.checked_add(1) {
                    // No overflow
                    Some(i) => i.into(),
                    // Overflow, number will fit in a i128
                    None => DiscriminantValue::from(i128::from(*i)).increment(),
                }
            }
            DiscriminantValue::I128(i) => {
                match i.checked_add_unsigned(1) {
                    // No overflow
                    Some(i) => i.into(),
                    // Overflow, number will fit in a u128
                    None => DiscriminantValue::from(u128::try_from(*i).unwrap()).increment(),
                }
            }
            DiscriminantValue::U128(i) => (i + 1).into(),
        }
    }
}

impl fmt::Display for DiscriminantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiscriminantValue::I64(i) => write!(f, "{}", i),
            DiscriminantValue::U64(i) => write!(f, "{}", i),
            DiscriminantValue::I128(i) => write!(f, "{}", i),
            DiscriminantValue::U128(i) => write!(f, "{}", i),
        }
    }
}

impl From<i64> for DiscriminantValue {
    fn from(value: i64) -> Self {
        DiscriminantValue::I64(value)
    }
}

impl From<i128> for DiscriminantValue {
    fn from(value: i128) -> Self {
        DiscriminantValue::I128(value)
    }
}

impl From<u64> for DiscriminantValue {
    fn from(value: u64) -> Self {
        DiscriminantValue::U64(value)
    }
}

impl From<u128> for DiscriminantValue {
    fn from(value: u128) -> Self {
        DiscriminantValue::U128(value)
    }
}

impl FromStr for DiscriminantValue {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(i) = i64::from_str(s) {
            return Ok(i.into());
        }
        if let Ok(i) = u64::from_str(s) {
            return Ok(i.into());
        }
        if let Ok(i) = i128::from_str(s) {
            return Ok(i.into());
        }
        match u128::from_str(s) {
            Ok(i) => Ok(i.into()),
            Err(e) => Err(e),
        }
    }
}

/// <https://doc.rust-lang.org/reference/items/enumerations.html#assigning-discriminant-values>
pub(super) fn assign_discriminants(variants: &Vec<&Variant>) -> Vec<String> {
    let mut last: DiscriminantValue = DiscriminantValue::I64(0);
    let mut discriminants: Vec<String> = Vec::with_capacity(variants.len());
    for v in variants {
        discriminants.push(match &v.discriminant {
            Some(d) => {
                last = DiscriminantValue::from_str(&d.value).unwrap();
                d.value.clone()
            }
            None => last.to_string(),
        });
        if !last.max() {
            last = last.increment();
        }
    }
    discriminants
}

#[cfg(test)]
mod tests {
    use rustdoc_types::{Discriminant, VariantKind};

    use super::*;

    #[test]
    fn i64() {
        let explicit_1 = Variant {
            discriminant: Some(Discriminant {
                value: "5".into(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let explicit_2 = Variant {
            discriminant: Some(Discriminant {
                value: "7".into(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let explicit_3 = Variant {
            discriminant: Some(Discriminant {
                value: "-59999".into(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let variants = vec![
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &explicit_1,
            &explicit_2,
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &explicit_3,
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
        ];
        let actual = assign_discriminants(&variants);
        let expected: Vec<String> = vec![
            "0".into(),
            "1".into(),
            "5".into(),
            "7".into(),
            "8".into(),
            "-59999".into(),
            "-59998".into(),
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn max() {
        let explicit_1 = Variant {
            discriminant: Some(Discriminant {
                value: i64::MAX.to_string(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let explicit_2 = Variant {
            discriminant: Some(Discriminant {
                value: u64::MAX.to_string(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let explicit_3 = Variant {
            discriminant: Some(Discriminant {
                value: i128::MAX.to_string(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let explicit_4 = Variant {
            discriminant: Some(Discriminant {
                value: u128::MAX.to_string(),
                expr: "".into(),
            }),
            kind: VariantKind::Plain,
        };
        let variants = vec![
            &explicit_1,
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &explicit_2,
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &explicit_3,
            &Variant {
                discriminant: None,
                kind: VariantKind::Plain,
            },
            &explicit_4,
        ];
        let actual = assign_discriminants(&variants);
        let expected: Vec<String> = vec![
            "9223372036854775807".into(),
            "9223372036854775808".into(),
            "18446744073709551615".into(),
            "18446744073709551616".into(),
            "170141183460469231731687303715884105727".into(),
            "170141183460469231731687303715884105728".into(),
            "340282366920938463463374607431768211455".into(),
        ];
        assert_eq!(actual, expected);
    }
}
