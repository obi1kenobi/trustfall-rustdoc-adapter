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
    /// Tries to parse `raw` as a comma-separated sequence of `AttributeMetaItem`'s
    /// wrapped in parentheses, square brackets or curly brackets.
    fn slice_arguments(raw: &'a str) -> Option<Vec<Rc<AttributeMetaItem<'a>>>> {
        let is_left = |c| c == '(' || c == '[' || c == '{';
        let is_right = |c| c == ')' || c == ']' || c == '}';
        let matching_right = |c| match c {
            '(' => ')',
            '[' => ']',
            '{' => '}',
            _ => unreachable!("Tried to find matching right bracket for {c}."),
        };

        let raw_trimmed = raw.trim();
        let first_char = raw_trimmed.chars().next()?;
        let raw_meta_seq = raw_trimmed
            .strip_prefix(is_left)?
            .strip_suffix(|c| c == matching_right(first_char))?
            .trim();

        let mut i = 0; // index of the first character after the last comma
        let mut bracket_depth = 0; // number of currently opened brackets
        let mut arguments: Vec<Rc<AttributeMetaItem>> = Vec::new(); // meta items constructed so far

        for (j, c) in raw_meta_seq.chars().enumerate() {
            if is_left(c) {
                bracket_depth += 1;
            } else if is_right(c) {
                bracket_depth -= 1;
            } else if c == ',' {
                // We only do a recursive call when the comma is on the outermost level.
                if bracket_depth == 0 {
                    arguments.push(Rc::new(AttributeMetaItem::new(&raw_meta_seq[i..j])));
                    i = j + 1;
                }
            }
        }

        // If the last comma was not a trailing one, there is still one meta item left.
        if i < raw_meta_seq.len() {
            arguments.push(Rc::new(AttributeMetaItem::new(&raw_meta_seq[i..])));
        }

        Some(arguments)
    }

    pub fn new(raw: &'a str) -> Self {
        let simple_path_char = |c: char| c.is_alphanumeric() || c == '_' || c == ':';
        let raw_trimmed = raw.trim();

        if let Some(path_end) = raw_trimmed.find(|c| !simple_path_char(c)) {
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
}
