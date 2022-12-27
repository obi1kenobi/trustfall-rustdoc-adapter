use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute<'a> {
    pub is_inner: bool,
    pub content: Rc<AttributeMetaItem<'a>>,
}

impl<'a> Attribute<'a> {
    pub fn raw_attribute(&self) -> String {
        format!(
            "#{}[{}]",
            if self.is_inner { "!" } else { "" },
            self.content.raw_item
        )
    }

    pub fn new(raw: &'a str) -> Self {
        let raw_trimmed = raw.trim();
        let raw_without_closing = raw_trimmed.strip_suffix(']').unwrap_or_else(|| {
            panic!(
                "\
String `{raw_trimmed}` cannot be parsed as an attribute \
because it is not closed with a square bracket."
            )
        });

        if let Some(raw_content) = raw_without_closing.strip_prefix("#[") {
            Attribute {
                is_inner: false,
                content: Rc::new(AttributeMetaItem::new(raw_content)),
            }
        } else if let Some(raw_content) = raw_without_closing.strip_prefix("#![") {
            Attribute {
                is_inner: true,
                content: Rc::new(AttributeMetaItem::new(raw_content)),
            }
        } else {
            panic!(
                "\
String `{raw_trimmed}` cannot be parsed as an attribute \
because it starts with neither `#[` nor `#![`."
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttributeMetaItem<'a> {
    pub raw_item: &'a str,
    pub base: &'a str,
    pub assigned_item: Option<&'a str>,
    pub arguments: Option<Vec<Rc<AttributeMetaItem<'a>>>>,
}

impl<'a> AttributeMetaItem<'a> {
    fn is_left_bracket(c: char) -> bool {
        c == '(' || c == '[' || c == '{'
    }

    fn is_right_bracket(c: char) -> bool {
        c == ')' || c == ']' || c == '}'
    }

    fn matching_right_bracket(c: char) -> char {
        match c {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => unreachable!("Tried to find matching right bracket for {c}."),
        }
    }

    /// Tries to parse `raw` as a comma-separated sequence of `AttributeMetaItem`'s
    /// wrapped in parentheses, square brackets or curly brackets.
    fn slice_arguments(raw: &'a str) -> Option<Vec<Rc<AttributeMetaItem<'a>>>> {
        let raw_trimmed = raw.trim();
        let first_char = raw_trimmed.chars().next()?;
        let raw_meta_seq = raw_trimmed
            .strip_prefix(Self::is_left_bracket)?
            .strip_suffix(|c| c == Self::matching_right_bracket(first_char))?
            .trim();

        let mut index_after_last_comma = 0;
        let mut previous_is_escape = false;
        let mut inside_string_literal = false;
        let mut brackets = Vec::new(); // currently opened brackets
        let mut arguments: Vec<Rc<AttributeMetaItem>> = Vec::new(); // meta items constructed so far

        for (j, c) in raw_meta_seq.char_indices() {
            if c == '"' && !previous_is_escape {
                inside_string_literal = !inside_string_literal;
            }

            if !inside_string_literal {
                if Self::is_left_bracket(c) {
                    brackets.push(c);
                } else if Self::is_right_bracket(c) {
                    // If the brackets don't match in any way, give up on parsing
                    // individual arguments since we don't understand the format.
                    if let Some(top_left) = brackets.pop() {
                        if Self::matching_right_bracket(top_left) != c {
                            return None;
                        }
                    } else {
                        return None;
                    }
                } else if c == ',' {
                    // We only do a recursive call when the comma is on the outermost level.
                    if brackets.is_empty() {
                        arguments.push(Rc::new(AttributeMetaItem::new(
                            &raw_meta_seq[index_after_last_comma..j],
                        )));
                        index_after_last_comma = j + 1;
                    }
                }
            }

            previous_is_escape = c == '\\';
        }

        // If the last comma was not a trailing one, there is still one meta item left.
        if index_after_last_comma < raw_meta_seq.len() {
            arguments.push(Rc::new(AttributeMetaItem::new(
                &raw_meta_seq[index_after_last_comma..],
            )));
        }

        Some(arguments)
    }

    pub fn new(raw: &'a str) -> Self {
        let raw_trimmed = raw.trim();

        if let Some(path_end) =
            raw_trimmed.find(|c: char| c.is_whitespace() || c == '=' || Self::is_left_bracket(c))
        {
            let simple_path = &raw_trimmed[0..path_end];
            let attr_input = &raw_trimmed[path_end..];
            if !simple_path.is_empty() {
                if let Some(assigned) = attr_input.trim().strip_prefix('=') {
                    return AttributeMetaItem {
                        raw_item: raw_trimmed,
                        base: simple_path,
                        assigned_item: Some(assigned.trim_start()),
                        arguments: None,
                    };
                } else if let Some(arguments) = Self::slice_arguments(attr_input) {
                    return AttributeMetaItem {
                        raw_item: raw_trimmed,
                        base: simple_path,
                        assigned_item: None,
                        arguments: Some(arguments),
                    };
                }
            }
        }

        AttributeMetaItem {
            raw_item: raw_trimmed,
            base: raw_trimmed,
            assigned_item: None,
            arguments: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::{Attribute, AttributeMetaItem};

    #[test]
    fn attribute_simple_inner() {
        let attribute = Attribute::new("#![no_std]");
        assert_eq!(
            attribute,
            Attribute {
                is_inner: true,
                content: Rc::new(AttributeMetaItem {
                    raw_item: "no_std",
                    base: "no_std",
                    assigned_item: None,
                    arguments: None
                })
            }
        );
        assert_eq!(attribute.raw_attribute(), "#![no_std]");
    }

    #[test]
    fn attribute_complex_outer() {
        let attribute =
            Attribute::new("#[cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))]");
        assert_eq!(
            attribute,
            Attribute {
                is_inner: false,
                content: Rc::new(AttributeMetaItem {
                    raw_item: "cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))",
                    base: "cfg_attr",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(AttributeMetaItem {
                            raw_item: "feature = \"serde\"",
                            base: "feature",
                            assigned_item: Some("\"serde\""),
                            arguments: None
                        }),
                        Rc::new(AttributeMetaItem {
                            raw_item: "derive(Serialize, Deserialize)",
                            base: "derive",
                            assigned_item: None,
                            arguments: Some(vec![
                                Rc::new(AttributeMetaItem {
                                    raw_item: "Serialize",
                                    base: "Serialize",
                                    assigned_item: None,
                                    arguments: None
                                }),
                                Rc::new(AttributeMetaItem {
                                    raw_item: "Deserialize",
                                    base: "Deserialize",
                                    assigned_item: None,
                                    arguments: None
                                })
                            ])
                        })
                    ])
                })
            }
        );
    }

    #[test]
    fn attribute_unformatted() {
        let attribute = Attribute::new("\t#[ derive ( Eq\t, PartialEq,   ) ]  ");
        assert_eq!(
            attribute,
            Attribute {
                is_inner: false,
                content: Rc::new(AttributeMetaItem {
                    raw_item: "derive ( Eq\t, PartialEq,   )",
                    base: "derive",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(AttributeMetaItem {
                            raw_item: "Eq",
                            base: "Eq",
                            assigned_item: None,
                            arguments: None
                        }),
                        Rc::new(AttributeMetaItem {
                            raw_item: "PartialEq",
                            base: "PartialEq",
                            assigned_item: None,
                            arguments: None
                        })
                    ])
                })
            }
        );
        assert_eq!(
            attribute.raw_attribute(),
            "#[derive ( Eq\t, PartialEq,   )]"
        );
    }

    #[test]
    fn attribute_utf8() {
        let attribute = Attribute::new("#[crate::gę42(bęc = \"🦀\", cśś = \"⭐\")]");
        assert_eq!(
            attribute,
            Attribute {
                is_inner: false,
                content: Rc::new(AttributeMetaItem {
                    raw_item: "crate::gę42(bęc = \"🦀\", cśś = \"⭐\")",
                    base: "crate::gę42",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(AttributeMetaItem {
                            raw_item: "bęc = \"🦀\"",
                            base: "bęc",
                            assigned_item: Some("\"🦀\""),
                            arguments: None
                        }),
                        Rc::new(AttributeMetaItem {
                            raw_item: "cśś = \"⭐\"",
                            base: "cśś",
                            assigned_item: Some("\"⭐\""),
                            arguments: None
                        })
                    ])
                })
            }
        )
    }

    #[test]
    fn attribute_raw_identifier() {
        let attribute = Attribute::new("#[r#derive(Debug)]");
        assert_eq!(
            attribute,
            Attribute {
                is_inner: false,
                content: Rc::new(AttributeMetaItem {
                    raw_item: "r#derive(Debug)",
                    base: "r#derive",
                    assigned_item: None,
                    arguments: Some(vec![Rc::new(AttributeMetaItem {
                        raw_item: "Debug",
                        base: "Debug",
                        assigned_item: None,
                        arguments: None
                    })])
                })
            }
        )
    }

    #[test]
    fn attribute_meta_item_custom_brackets() {
        for raw_attribute in ["macro{arg1,arg2}", "macro[arg1,arg2]"] {
            let meta_item = AttributeMetaItem::new(raw_attribute);
            assert_eq!(
                meta_item,
                AttributeMetaItem {
                    raw_item: raw_attribute,
                    base: "macro",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(AttributeMetaItem {
                            raw_item: "arg1",
                            base: "arg1",
                            assigned_item: None,
                            arguments: None
                        }),
                        Rc::new(AttributeMetaItem {
                            raw_item: "arg2",
                            base: "arg2",
                            assigned_item: None,
                            arguments: None
                        })
                    ])
                }
            );
        }
    }

    #[test]
    fn attribute_meta_item_unrecognized_form() {
        let meta_item = AttributeMetaItem::new("foo|bar|");
        assert_eq!(
            meta_item,
            AttributeMetaItem {
                raw_item: "foo|bar|",
                base: "foo|bar|",
                assigned_item: None,
                arguments: None
            }
        );
    }

    #[test]
    fn attribute_meta_item_string_literals() {
        let literals = [
            " ",
            "comma ,",
            "comma , escaped quote \\\" right parenthesis ) ",
            "right parenthesis ) comma , left parenthesis (",
            "right square ) comma , left square (",
            "right curly } comma , left curly {",
            "Mężny bądź, chroń pułk twój i sześć flag.",
        ];

        for literal in literals {
            let raw_attribute = format!("foo(bar = \"{literal}\", baz = \"{literal}\")");
            let meta_item = AttributeMetaItem::new(&raw_attribute);
            assert_eq!(
                meta_item,
                AttributeMetaItem {
                    raw_item: &raw_attribute,
                    base: "foo",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(AttributeMetaItem {
                            raw_item: format!("bar = \"{literal}\"").as_str(),
                            base: "bar",
                            assigned_item: Some(format!("\"{literal}\"").as_str()),
                            arguments: None
                        }),
                        Rc::new(AttributeMetaItem {
                            raw_item: format!("baz = \"{literal}\"").as_str(),
                            base: "baz",
                            assigned_item: Some(format!("\"{literal}\"").as_str()),
                            arguments: None
                        })
                    ])
                }
            )
        }
    }
}
