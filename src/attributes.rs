use std::{
    borrow::Cow,
    rc::Rc,
    sync::{Arc, LazyLock},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute<'a> {
    Structured(&'a rustdoc_types::Attribute),
    Parsed(ParsedAttribute<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeMetaItem<'a> {
    Structured(usize, usize, &'a rustdoc_types::Attribute), // (depth, index, root attr)
    Parsed(Rc<ParsedAttributeMetaItem<'a>>),
}

impl<'a> Attribute<'a> {
    pub(crate) fn new(attr: &'a rustdoc_types::Attribute) -> Self {
        match attr {
            rustdoc_types::Attribute::Other(attr) => Self::Parsed(ParsedAttribute::new(attr)),
            _ => Self::Structured(attr),
        }
    }

    pub(crate) fn content(&self) -> AttributeMetaItem<'a> {
        match self {
            Attribute::Structured(attr) => AttributeMetaItem::Structured(0, 0, attr),
            Attribute::Parsed(parsed) => AttributeMetaItem::Parsed(parsed.content.clone()),
        }
    }

    pub(crate) fn is_inner(&self) -> bool {
        match self {
            Attribute::Structured(..) => false, // structured attributes are always in outer form
            Attribute::Parsed(parsed) => parsed.is_inner,
        }
    }

    pub(crate) fn raw_attribute(&self) -> Arc<str> {
        match self {
            Attribute::Structured(attr) => structured_attr_to_arc_str(attr),
            Attribute::Parsed(parsed) => parsed.raw_attribute().into(),
        }
    }

    /// Checks if the attribute contents contain `#[doc(hidden)]`.
    ///
    /// Also returns `true` if the "hidden" argument is combined with other arguments.
    pub(crate) fn is_doc_hidden(attr: &rustdoc_types::Attribute) -> bool {
        let rustdoc_types::Attribute::Other(raw) = attr else {
            // This is some other kind of attribute.
            return false;
        };

        // We cannot just look for `#[doc(hidden)]` as a string,
        // since it might be combined with other arguments to `#[doc]`.
        //
        // However, we'd like to bail early without parsing the full attribute if possible,
        // since parsing is expensive and involves many allocations.
        // We can rely on the fact that rustdoc does some formatting and normalization
        // of the attributes presented in rustdoc JSON, for example removing unnecessary spaces.
        let raw = raw.trim_start();
        if !raw.starts_with("#[doc(") {
            return false;
        }

        let attribute = ParsedAttribute::new(raw);

        // We look for:
        // - the base of the attribute is `doc`, and
        // - one of its arguments has the base `hidden`.
        //
        // This gracefully handles complex cases like `#[doc(hidden, alias = "TheAlias")]`.
        attribute.content.base == "doc"
            && attribute
                .content
                .arguments
                .iter()
                .flatten()
                .any(|arg| arg.base == "hidden")
    }
}

static NON_EXHAUSTIVE: LazyLock<Arc<str>> = LazyLock::new(|| "non_exhaustive".into());
static AUTOMATICALLY_DERIVED: LazyLock<Arc<str>> = LazyLock::new(|| "automatically_derived".into());
static MUST_USE: LazyLock<Arc<str>> = LazyLock::new(|| "must_use".into());
static NO_MANGLE: LazyLock<Arc<str>> = LazyLock::new(|| "no_mangle".into());
static EXPORT_NAME: LazyLock<Arc<str>> = LazyLock::new(|| "export_name".into());
static LINK_SECTION: LazyLock<Arc<str>> = LazyLock::new(|| "link_section".into());
static REPR: LazyLock<Arc<str>> = LazyLock::new(|| "repr".into());
static REPR_C: LazyLock<Arc<str>> = LazyLock::new(|| "C".into());
static REPR_TRANSPARENT: LazyLock<Arc<str>> = LazyLock::new(|| "transparent".into());
static REPR_SIMD: LazyLock<Arc<str>> = LazyLock::new(|| "simd".into());
static PACKED: LazyLock<Arc<str>> = LazyLock::new(|| "packed".into());
static ALIGN: LazyLock<Arc<str>> = LazyLock::new(|| "align".into());
static TARGET_FEATURE: LazyLock<Arc<str>> = LazyLock::new(|| "target_feature".into());
static ENABLE: LazyLock<Arc<str>> = LazyLock::new(|| "enable".into());
static MACRO_EXPORT: LazyLock<Arc<str>> = LazyLock::new(|| "macro_export".into());

impl<'a> AttributeMetaItem<'a> {
    const REPR_KIND_INDEX: usize = 0;
    const REPR_INT_INDEX: usize = 1;
    const REPR_PACKED_INDEX: usize = 2;
    const REPR_ALIGN_INDEX: usize = 3;

    pub(crate) fn arguments(&self) -> Vec<AttributeMetaItem<'a>> {
        match self {
            AttributeMetaItem::Structured(depth, index, attribute) => {
                match attribute {
                    rustdoc_types::Attribute::NonExhaustive |
                    rustdoc_types::Attribute::AutomaticallyDerived |
                    rustdoc_types::Attribute::MustUse { .. } |
                    // The following attributes have `unsafe()` and non-unsafe forms,
                    // depending on edition. In the structured form, we don't know
                    // which form was used. Pretend it's the non-unsafe one, for efficiency.
                    rustdoc_types::Attribute::NoMangle |
                    rustdoc_types::Attribute::ExportName(_) |
                    rustdoc_types::Attribute::LinkSection(_) |
                    // End of unsafe attributes.
                    rustdoc_types::Attribute::MacroExport => Vec::new(),
                    rustdoc_types::Attribute::Repr(attribute_repr) => match depth {
                        0 => {
                            let mut output = Vec::with_capacity(3);
                            if !matches!(attribute_repr.kind, rustdoc_types::ReprKind::Rust) {
                                output.push(Self::Structured(1, Self::REPR_KIND_INDEX, attribute));
                            }

                            if attribute_repr.int.is_some() {
                                output.push(Self::Structured(1, Self::REPR_INT_INDEX, attribute));
                            }

                            if attribute_repr.packed.is_some() {
                                output.push(Self::Structured(1, Self::REPR_PACKED_INDEX, attribute));
                            }

                            if attribute_repr.align.is_some() {
                                output.push(Self::Structured(1, Self::REPR_ALIGN_INDEX, attribute));
                            }

                            output
                        }
                        1 => match *index {
                            Self::REPR_PACKED_INDEX => vec![Self::Structured(2, *index, attribute)],
                            Self::REPR_ALIGN_INDEX => vec![Self::Structured(2, *index, attribute)],
                            _ => unreachable!("unexpected index {index} at depth {depth} for {attribute:?}"),
                        }
                        _ => {
                            // In case someone calls `arguments()` at the innermost layer of
                            // an attribute like `repr(packed(1))` or `repr(align(16))`.
                            Vec::new()
                        }
                    }
                    rustdoc_types::Attribute::TargetFeature { enable: _enable } => match depth {
                        0 => vec![Self::Structured(1, 0, attribute)], // The `enable` meta item.
                        _ => Vec::new(),  // The `enable` features are assigned, not attributes.
                    }
                    rustdoc_types::Attribute::Other(_) => unreachable!("attribute needing parsing found in structured path: {self:?}"),
                }
            }
            AttributeMetaItem::Parsed(parsed_attribute_meta_item) => parsed_attribute_meta_item
                .arguments
                .iter()
                .flatten()
                .cloned()
                .map(AttributeMetaItem::Parsed)
                .collect(),
        }
    }

    pub(crate) fn raw_item(&self) -> Arc<str> {
        match self {
            AttributeMetaItem::Structured(depth, index, attribute) => match attribute {
                rustdoc_types::Attribute::NonExhaustive => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&NON_EXHAUSTIVE)
                }
                rustdoc_types::Attribute::MustUse { reason } => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    match reason {
                        None => Arc::clone(&MUST_USE),
                        Some(r) => format!("must_use = {}", escape_quotes(r.as_str())).into(),
                    }
                }
                rustdoc_types::Attribute::AutomaticallyDerived => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&AUTOMATICALLY_DERIVED)
                }
                // For the next 3 attributes, we always pretend they are in non-unsafe form
                // for simplicity of the pretend-parsing.
                rustdoc_types::Attribute::ExportName(exp) => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    format!("export_name = {}", escape_quotes(exp.as_str())).into()
                }
                rustdoc_types::Attribute::LinkSection(link) => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    format!("link_section = {}", escape_quotes(link.as_str())).into()
                }
                rustdoc_types::Attribute::NoMangle => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&NO_MANGLE)
                }
                rustdoc_types::Attribute::Repr(attribute_repr) => match *depth {
                    0 => {
                        let mut builder = String::with_capacity(24);
                        add_repr_contents(&mut builder, attribute_repr);
                        builder.into()
                    }
                    1 => match *index {
                        Self::REPR_PACKED_INDEX => {
                            let packed = attribute_repr.packed.unwrap_or_else(|| {
                                panic!("no packed attr when getting raw item: {self:?}")
                            });
                            format!("packed({packed})").into()
                        }
                        Self::REPR_ALIGN_INDEX => {
                            let align = attribute_repr.align.unwrap_or_else(|| {
                                panic!("no align attr when getting raw item: {self:?}")
                            });
                            format!("align({align})").into()
                        }
                        _ => unreachable!(
                            "unexpected index {index} at depth {depth} for {attribute:?}"
                        ),
                    },
                    2 => match *index {
                        Self::REPR_PACKED_INDEX => {
                            let packed = attribute_repr.packed.unwrap_or_else(|| {
                                panic!("no packed attr when getting raw item: {self:?}")
                            });
                            packed.to_string().into()
                        }
                        Self::REPR_ALIGN_INDEX => {
                            let align = attribute_repr.align.unwrap_or_else(|| {
                                panic!("no align attr when getting raw item: {self:?}")
                            });
                            align.to_string().into()
                        }
                        _ => unreachable!(
                            "unexpected index {index} at depth {depth} for {attribute:?}"
                        ),
                    },
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::TargetFeature { enable } => match *depth {
                    0 => format!("target_feature(enable = \"{}\")", enable.join(",")).into(),
                    1 => format!("enable = \"{}\"", enable.join(",")).into(),
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::MacroExport => Arc::clone(&MACRO_EXPORT),
                rustdoc_types::Attribute::Other(_) => {
                    unreachable!("attribute needing parsing found in structured path: {self:?}")
                }
            },
            AttributeMetaItem::Parsed(parsed) => parsed.raw_item.into(),
        }
    }

    pub(crate) fn base(&self) -> Arc<str> {
        match self {
            AttributeMetaItem::Structured(depth, index, attribute) => match attribute {
                rustdoc_types::Attribute::NonExhaustive => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&NON_EXHAUSTIVE)
                }
                rustdoc_types::Attribute::MustUse { .. } => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&MUST_USE)
                }
                rustdoc_types::Attribute::AutomaticallyDerived => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&AUTOMATICALLY_DERIVED)
                }
                // For the next 3 attributes, we always pretend they are in non-unsafe form
                // for simplicity of the pretend-parsing.
                rustdoc_types::Attribute::ExportName(..) => match *depth {
                    0 => Arc::clone(&EXPORT_NAME),
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::LinkSection(..) => match *depth {
                    0 => Arc::clone(&LINK_SECTION),
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::NoMangle => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Arc::clone(&NO_MANGLE)
                }
                rustdoc_types::Attribute::Repr(attribute_repr) => match *depth {
                    0 => Arc::clone(&REPR),
                    1 => match *index {
                        Self::REPR_KIND_INDEX => match attribute_repr.kind {
                            rustdoc_types::ReprKind::Rust => {
                                panic!("looking up kind in Rust repr: {self:?}");
                            }
                            rustdoc_types::ReprKind::C => Arc::clone(&REPR_C),
                            rustdoc_types::ReprKind::Transparent => Arc::clone(&REPR_TRANSPARENT),
                            rustdoc_types::ReprKind::Simd => Arc::clone(&REPR_SIMD),
                        },
                        Self::REPR_INT_INDEX => match &attribute_repr.int {
                            None => panic!("looking up int repr in repr without it: {self:?}"),
                            Some(repr) => Arc::from(repr.as_str()),
                        },
                        Self::REPR_PACKED_INDEX => {
                            assert!(
                                attribute_repr.packed.is_some(),
                                "looking up packed in repr without it: {self:?}"
                            );
                            Arc::clone(&PACKED)
                        }
                        Self::REPR_ALIGN_INDEX => {
                            assert!(
                                attribute_repr.align.is_some(),
                                "looking up align in repr without it: {self:?}"
                            );
                            Arc::clone(&ALIGN)
                        }
                        _ => unreachable!(
                            "unexpected index {index} at depth {depth} for {attribute:?}"
                        ),
                    },
                    2 => match *index {
                        Self::REPR_PACKED_INDEX => {
                            let packed = attribute_repr.packed.unwrap_or_else(|| {
                                panic!("no packed attr when getting raw item: {self:?}")
                            });
                            packed.to_string().into()
                        }
                        Self::REPR_ALIGN_INDEX => {
                            let align = attribute_repr.align.unwrap_or_else(|| {
                                panic!("no align attr when getting raw item: {self:?}")
                            });
                            align.to_string().into()
                        }
                        _ => unreachable!(
                            "unexpected index {index} at depth {depth} for {attribute:?}"
                        ),
                    },
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::TargetFeature { enable: _enable } => match *depth {
                    0 => Arc::clone(&TARGET_FEATURE),
                    1 => Arc::clone(&ENABLE),
                    _ => unreachable!("invalid attr item: {self:?}"),
                },
                rustdoc_types::Attribute::MacroExport => Arc::clone(&MACRO_EXPORT),
                rustdoc_types::Attribute::Other(_) => {
                    unreachable!("attribute needing parsing found in structured path: {self:?}")
                }
            },
            AttributeMetaItem::Parsed(parsed) => parsed.base.into(),
        }
    }

    pub(crate) fn assigned_item(&self) -> Option<Arc<str>> {
        match self {
            AttributeMetaItem::Structured(depth, _index, attribute) => match attribute {
                rustdoc_types::Attribute::NonExhaustive
                | rustdoc_types::Attribute::AutomaticallyDerived
                | rustdoc_types::Attribute::NoMangle
                | rustdoc_types::Attribute::MacroExport => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    None
                }
                rustdoc_types::Attribute::MustUse { reason } => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    reason.as_deref().map(Into::into)
                }
                rustdoc_types::Attribute::ExportName(inner)
                | rustdoc_types::Attribute::LinkSection(inner) => {
                    assert_eq!(*depth, 0, "invalid depth {depth} for attr item {self:?}");
                    Some(inner.as_str().into())
                }
                rustdoc_types::Attribute::Repr(..) => {
                    // None of the `#[repr]` attributes use assignment notation.
                    None
                }
                rustdoc_types::Attribute::TargetFeature { enable } => match *depth {
                    0 => None, // `#[target_feature]` has `enable` in argument position.
                    1 => Some(enable.join(",").into()),
                    _ => None,
                },
                rustdoc_types::Attribute::Other(_) => {
                    unreachable!("attribute needing parsing found in structured path: {self:?}")
                }
            },
            AttributeMetaItem::Parsed(parsed) => parsed.assigned_item.map(Into::into),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedAttribute<'a> {
    is_inner: bool,
    content: Rc<ParsedAttributeMetaItem<'a>>,
}

impl<'a> ParsedAttribute<'a> {
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
            ParsedAttribute {
                is_inner: false,
                content: Rc::new(ParsedAttributeMetaItem::new(raw_content)),
            }
        } else if let Some(raw_content) = raw_without_closing.strip_prefix("#![") {
            ParsedAttribute {
                is_inner: true,
                content: Rc::new(ParsedAttributeMetaItem::new(raw_content)),
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
pub struct ParsedAttributeMetaItem<'a> {
    raw_item: &'a str,
    base: &'a str,
    assigned_item: Option<&'a str>,
    arguments: Option<Vec<Rc<ParsedAttributeMetaItem<'a>>>>,
}

impl<'a> ParsedAttributeMetaItem<'a> {
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
    fn slice_arguments(raw: &'a str) -> Option<Vec<Rc<Self>>> {
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
        let mut arguments: Vec<Rc<ParsedAttributeMetaItem<'a>>> = Vec::new(); // meta items constructed so far

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
                        arguments.push(Rc::new(ParsedAttributeMetaItem::new(
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
            arguments.push(Rc::new(ParsedAttributeMetaItem::new(
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
                    return ParsedAttributeMetaItem {
                        raw_item: raw_trimmed,
                        base: simple_path,
                        assigned_item: Some(assigned.trim_start()),
                        arguments: None,
                    };
                } else if let Some(arguments) = Self::slice_arguments(attr_input) {
                    return ParsedAttributeMetaItem {
                        raw_item: raw_trimmed,
                        base: simple_path,
                        assigned_item: None,
                        arguments: Some(arguments),
                    };
                }
            }
        }

        ParsedAttributeMetaItem {
            raw_item: raw_trimmed,
            base: raw_trimmed,
            assigned_item: None,
            arguments: None,
        }
    }
}

pub(crate) fn structured_attr_to_arc_str(attr: &rustdoc_types::Attribute) -> Arc<str> {
    static NON_EXHAUSTIVE_ATTR: LazyLock<Arc<str>> =
        LazyLock::new(|| Arc::from("#[non_exhaustive]"));
    static AUTOMATICALLY_DERIVED_ATTR: LazyLock<Arc<str>> =
        LazyLock::new(|| Arc::from("#[automatically_derived]"));
    static NO_MANGLE_ATTR: LazyLock<Arc<str>> = LazyLock::new(|| Arc::from("#[no_mangle]"));
    static MUST_USE_ATTR: LazyLock<Arc<str>> = LazyLock::new(|| Arc::from("#[must_use]"));
    static MACRO_EXPORT_ATTR: LazyLock<Arc<str>> = LazyLock::new(|| Arc::from("#[macro_export]"));

    match attr {
        rustdoc_types::Attribute::NonExhaustive => Arc::clone(&NON_EXHAUSTIVE_ATTR),
        rustdoc_types::Attribute::MustUse { reason } => match reason {
            None => Arc::clone(&MUST_USE_ATTR),
            Some(x) => format!("#[must_use = \"{}\"]", escape_quotes(x)).into(),
        },
        rustdoc_types::Attribute::ExportName(x) => {
            format!("#[export_name = \"{}\"]", escape_quotes(x)).into()
        }
        rustdoc_types::Attribute::LinkSection(x) => {
            format!("#[link_section = \"{}\"]", escape_quotes(x)).into()
        }
        rustdoc_types::Attribute::AutomaticallyDerived => Arc::clone(&AUTOMATICALLY_DERIVED_ATTR),
        rustdoc_types::Attribute::Repr(attribute_repr) => {
            let mut builder = String::with_capacity(32);
            builder.push_str("#[repr(");

            add_repr_contents(&mut builder, attribute_repr);

            builder.push_str(")]");

            builder.into()
        }
        rustdoc_types::Attribute::NoMangle => Arc::clone(&NO_MANGLE_ATTR),
        rustdoc_types::Attribute::TargetFeature { enable } => {
            format!("#[target_feature(enable = \"{}\")]", enable.join(",")).into()
        }
        rustdoc_types::Attribute::MacroExport => Arc::clone(&MACRO_EXPORT_ATTR),
        rustdoc_types::Attribute::Other(attr) => Arc::from(attr.as_str()),
    }
}

// Canonically, we choose the order: `kind, int, packed, align`.
fn add_repr_contents(builder: &mut String, attribute_repr: &rustdoc_types::AttributeRepr) {
    let mut need_separator = false;
    let separator = ", ";
    let kind = match attribute_repr.kind {
        rustdoc_types::ReprKind::Rust => None,
        rustdoc_types::ReprKind::C => Some("C"),
        rustdoc_types::ReprKind::Transparent => Some("transparent"),
        rustdoc_types::ReprKind::Simd => Some("simd"),
    };
    if let Some(kind) = kind {
        builder.push_str(kind);
        need_separator = true;
    }

    if let Some(repr_int) = attribute_repr.int.as_deref() {
        if need_separator {
            builder.push_str(separator);
        } else {
            need_separator = true;
        }

        builder.push_str(repr_int);
    }

    if let Some(packed) = attribute_repr.packed {
        if need_separator {
            builder.push_str(separator);
        } else {
            need_separator = true;
        }

        builder.push_str("packed(");
        builder.push_str(&packed.to_string());
        builder.push(')');
    }

    if let Some(align) = attribute_repr.align {
        if need_separator {
            builder.push_str(separator);
        }

        builder.push_str("align(");
        builder.push_str(&align.to_string());
        builder.push(')');
    }
}

fn escape_quotes(value: &str) -> Cow<'_, str> {
    if value.contains('"') {
        Cow::Owned(value.replace('"', r#"\""#))
    } else {
        Cow::Borrowed(value)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::attributes::{ParsedAttribute, ParsedAttributeMetaItem};

    use super::Attribute;

    #[test]
    fn is_doc_hidden() {
        let doc_hidden = Attribute::is_doc_hidden(&rustdoc_types::Attribute::Other(
            "#[doc(hidden, alias = \"TheAlias\")]".to_string(),
        ));
        assert!(doc_hidden);
    }

    #[test]
    fn attribute_simple_inner() {
        let attribute = ParsedAttribute::new("#![no_std]");
        assert_eq!(
            attribute,
            ParsedAttribute {
                is_inner: true,
                content: Rc::new(ParsedAttributeMetaItem {
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
        let attribute = ParsedAttribute::new(
            "#[cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))]",
        );
        assert_eq!(
            attribute,
            ParsedAttribute {
                is_inner: false,
                content: Rc::new(ParsedAttributeMetaItem {
                    raw_item: "cfg_attr(feature = \"serde\", derive(Serialize, Deserialize))",
                    base: "cfg_attr",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: "feature = \"serde\"",
                            base: "feature",
                            assigned_item: Some("\"serde\""),
                            arguments: None
                        }),
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: "derive(Serialize, Deserialize)",
                            base: "derive",
                            assigned_item: None,
                            arguments: Some(vec![
                                Rc::new(ParsedAttributeMetaItem {
                                    raw_item: "Serialize",
                                    base: "Serialize",
                                    assigned_item: None,
                                    arguments: None
                                }),
                                Rc::new(ParsedAttributeMetaItem {
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
        let attribute = ParsedAttribute::new("\t#[ derive ( Eq\t, PartialEq,   ) ]  ");
        assert_eq!(
            attribute,
            ParsedAttribute {
                is_inner: false,
                content: Rc::new(ParsedAttributeMetaItem {
                    raw_item: "derive ( Eq\t, PartialEq,   )",
                    base: "derive",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: "Eq",
                            base: "Eq",
                            assigned_item: None,
                            arguments: None
                        }),
                        Rc::new(ParsedAttributeMetaItem {
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
        let attribute = ParsedAttribute::new("#[crate::gę42(bęc = \"🦀\", cśś = \"⭐\")]");
        assert_eq!(
            attribute,
            ParsedAttribute {
                is_inner: false,
                content: Rc::new(ParsedAttributeMetaItem {
                    raw_item: "crate::gę42(bęc = \"🦀\", cśś = \"⭐\")",
                    base: "crate::gę42",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: "bęc = \"🦀\"",
                            base: "bęc",
                            assigned_item: Some("\"🦀\""),
                            arguments: None
                        }),
                        Rc::new(ParsedAttributeMetaItem {
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
        let attribute = ParsedAttribute::new("#[r#derive(Debug)]");
        assert_eq!(
            attribute,
            ParsedAttribute {
                is_inner: false,
                content: Rc::new(ParsedAttributeMetaItem {
                    raw_item: "r#derive(Debug)",
                    base: "r#derive",
                    assigned_item: None,
                    arguments: Some(vec![Rc::new(ParsedAttributeMetaItem {
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
            let meta_item = ParsedAttributeMetaItem::new(raw_attribute);
            assert_eq!(
                meta_item,
                ParsedAttributeMetaItem {
                    raw_item: raw_attribute,
                    base: "macro",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: "arg1",
                            base: "arg1",
                            assigned_item: None,
                            arguments: None
                        }),
                        Rc::new(ParsedAttributeMetaItem {
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
        let meta_item = ParsedAttributeMetaItem::new("foo|bar|");
        assert_eq!(
            meta_item,
            ParsedAttributeMetaItem {
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
            let meta_item = ParsedAttributeMetaItem::new(&raw_attribute);
            assert_eq!(
                meta_item,
                ParsedAttributeMetaItem {
                    raw_item: &raw_attribute,
                    base: "foo",
                    assigned_item: None,
                    arguments: Some(vec![
                        Rc::new(ParsedAttributeMetaItem {
                            raw_item: format!("bar = \"{literal}\"").as_str(),
                            base: "bar",
                            assigned_item: Some(format!("\"{literal}\"").as_str()),
                            arguments: None
                        }),
                        Rc::new(ParsedAttributeMetaItem {
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
