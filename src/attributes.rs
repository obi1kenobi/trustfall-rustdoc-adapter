use anyhow::bail;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub is_inner: bool,
    pub content: AttributeValue,
}

impl Attribute {
    pub fn as_string(&self) -> String {
        format!(
            "#{}[{}]",
            if self.is_inner { "!" } else { "" },
            self.content.as_string
        )
    }
}

impl<'a> TryFrom<&'a str> for Attribute {
    type Error = anyhow::Error;

    fn try_from(as_string: &'a str) -> anyhow::Result<Self> {
        lazy_static! {
            static ref INNER_RE: Regex = Regex::new(r"#!\[(.*)\]").unwrap();
            static ref OUTER_RE: Regex = Regex::new(r"#\[(.*)\]").unwrap();
        }
        match INNER_RE.captures(as_string) {
            Some(captures) => Ok(Attribute {
                is_inner: true,
                content: AttributeValue::try_from(&captures[1])?,
            }),
            None => match OUTER_RE.captures(as_string) {
                Some(captures) => Ok(Attribute {
                    is_inner: false,
                    content: AttributeValue::try_from(&captures[1])?,
                }),
                None => bail!("Attribute has to be in one of the following forms: `#[...]` or `#![...]`, but found: `{}`", as_string)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeValue {
    pub as_string: String,
    pub base: String,
    pub assigned_expression: Option<String>,
    pub arguments: Option<Vec<AttributeValue>>,
}

impl<'a> TryFrom<&'a str> for AttributeValue {
    type Error = anyhow::Error;

    // TODO: Are we sure it recognises all possible attributes? Maybe it's better
    // to ignore errors instead of panicing?
    fn try_from(as_string: &'a str) -> anyhow::Result<Self> {
        const PATH_RE_STR: &str = r"[[:blank:]]*(?P<simple_path>[[:word:]:]+)[[:blank:]]*";
        lazy_static! {
            static ref PATH_RE: Regex = Regex::new(format!(r"^{}$", PATH_RE_STR).as_str()).unwrap();
            static ref ASSIGNMENT_RE: Regex =
                Regex::new(format!(r"^{}=[[:blank:]]*(?P<expression>.*)$", PATH_RE_STR).as_str())
                    .unwrap();
            static ref ARGUMENTS_RE: Regex = Regex::new(
                format!(
                    r"^{}[\(\[\{{](?P<arguments>.*)[\)\]\}}][[:blank:]]*$",
                    PATH_RE_STR
                )
                .as_str()
            )
            .unwrap();
        }

        PATH_RE
            .captures(as_string)
            .map(|captures| AttributeValue {
                as_string: as_string.to_string(),
                base: captures["simple_path"].to_string(),
                assigned_expression: None,
                arguments: None,
            })
            .or_else(|| {
                ASSIGNMENT_RE
                    .captures(as_string)
                    .map(|captures| AttributeValue {
                        as_string: as_string.to_string(),
                        base: captures["simple_path"].to_string(),
                        assigned_expression: Some(captures["expression"].to_string()),
                        arguments: None,
                    })
            })
            .or_else(|| {
                ARGUMENTS_RE.captures(as_string).map(|captures| {
                    let mut i = 0;
                    let mut depth = 0;
                    let ref arg_str = captures["arguments"];
                    let mut arguments: Vec<AttributeValue> = Vec::new();
                    let mut only_white = true;
                    for (j, c) in arg_str.chars().enumerate() {
                        if c != ' ' && c != '\t' {
                            only_white = false;
                        }

                        if c == '(' || c == '[' || c == '{' {
                            depth += 1;
                        } else if c == ')' || c == ']' || c == '}' {
                            depth -= 1;
                        } else if c == ',' && depth == 0 {
                            arguments.push(AttributeValue::try_from(&arg_str[i..j]).unwrap());
                            i = j + 1;
                            only_white = true;
                        }
                    }
                    if i < arg_str.len() && !only_white {
                        arguments.push(AttributeValue::try_from(&arg_str[i..]).unwrap());
                    }

                    AttributeValue {
                        as_string: as_string.to_string(),
                        base: captures["simple_path"].to_string(),
                        assigned_expression: None,
                        arguments: Some(arguments),
                    }
                })
            })
            .ok_or(anyhow::anyhow!(
                "Unrecognized expression inside the attribute: `{}`",
                as_string
            ))
    }
}

#[cfg(test)]
mod tests {
    use super::{Attribute, AttributeValue};

    #[test]
    fn attribute_from_string_simple_inner() {
        let attribute = Attribute::try_from("#![no_std]").unwrap();
        assert_eq!(
            attribute,
            Attribute {
                is_inner: true,
                content: AttributeValue {
                    as_string: "no_std".to_string(),
                    base: "no_std".to_string(),
                    assigned_expression: None,
                    arguments: None
                }
            }
        );
        assert_eq!(attribute.as_string(), "#![no_std]");
    }

    #[test]
    fn attribute_from_string_complex_outer() {
        let attribute =
            Attribute::try_from("#[cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))]")
                .unwrap();
        assert_eq!(
            attribute,
            Attribute {
                is_inner: false,
                content: AttributeValue {
                    as_string: "cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))"
                        .to_string(),
                    base: "cfg_attr".to_string(),
                    assigned_expression: None,
                    arguments: Some(vec![
                        AttributeValue {
                            as_string: "feature = \"serde\"".to_string(),
                            base: "feature".to_string(),
                            assigned_expression: Some("\"serde\"".to_string()),
                            arguments: None
                        },
                        AttributeValue {
                            as_string: " derive(Serialize, Deserialize)".to_string(),
                            base: "derive".to_string(),
                            assigned_expression: None,
                            arguments: Some(vec![
                                AttributeValue {
                                    as_string: "Serialize".to_string(),
                                    base: "Serialize".to_string(),
                                    assigned_expression: None,
                                    arguments: None
                                },
                                AttributeValue {
                                    as_string: " Deserialize".to_string(),
                                    base: "Deserialize".to_string(),
                                    assigned_expression: None,
                                    arguments: None
                                }
                            ])
                        }
                    ])
                }
            }
        );
    }

    #[test]
    fn attribute_value_from_string_custom_brackets() {
        for as_string in ["macro{arg1,arg2}", "macro[arg1,arg2]"] {
            let attr_val = AttributeValue::try_from(as_string).unwrap();
            assert_eq!(
                attr_val,
                AttributeValue {
                    as_string: as_string.to_string(),
                    base: "macro".to_string(),
                    assigned_expression: None,
                    arguments: Some(vec![
                        AttributeValue {
                            as_string: "arg1".to_string(),
                            base: "arg1".to_string(),
                            assigned_expression: None,
                            arguments: None
                        },
                        AttributeValue {
                            as_string: "arg2".to_string(),
                            base: "arg2".to_string(),
                            assigned_expression: None,
                            arguments: None
                        }
                    ])
                }
            );
        }
    }
}
