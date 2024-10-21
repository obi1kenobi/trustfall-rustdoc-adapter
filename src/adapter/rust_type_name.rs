use std::fmt::{Display, Formatter, Result};

/// Serializes the `rustdoc_types::Type` type as a String containing the name
/// of the type, as close as possible to the original code declaration.
pub(crate) fn rust_type_name(ty: &rustdoc_types::Type) -> String {
    Type(ty, false).to_string()
}

/// Creates a struct named `$t` that wraps a `rustdoc_types::$t` reference,
/// and implements `Display` on it by calling the given `$formatter` function.
macro_rules! display_wrapper {
    ($vis:vis $t:ident, $formatter:ident $(,  $extra:ident)*) => {
        #[doc = concat!(
            "Wraps a &rustdoc_types::",
            stringify!($t),
            " to implement [`Display`]."
        )]
        #[derive(Debug, Clone, Copy)]
        $vis struct $t<'a>($vis &'a rustdoc_types::$t $(, $extra)*);

        impl Display for $t<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                $formatter(self, f)
            }
        }
    };
}

/// Formats a sequence of `items` using the given formatter and format function `f`,
/// putting the `delim` between each item (but with no leading or trailing delimiter).
fn intersperse<T, I: IntoIterator<Item = T>, F: FnMut(T, &mut Formatter<'_>) -> Result>(
    fmt: &mut Formatter<'_>,
    delim: &str,
    items: I,
    mut f: F,
) -> Result {
    let mut first = true;
    for item in items {
        if first {
            first = false;
        } else {
            write!(fmt, "{delim}")?;
        }

        f(item, fmt)?;
    }

    Ok(())
}

/// Calls [`intersperse`] with the given delimiter and items, using the `Display`
/// impl of the value returned by the given function.
fn intersperse_with<T, I: IntoIterator<Item = T>, U: Display, F: Fn(T) -> U>(
    fmt: &mut Formatter<'_>,
    delim: &str,
    items: I,
    f: F,
) -> Result {
    intersperse(fmt, delim, items, |x, fmt| f(x).fmt(fmt))
}

fn fmt_generic_param_def(this: &GenericParamDef, f: &mut Formatter<'_>) -> Result {
    match &this.0.kind {
        rustdoc_types::GenericParamDefKind::Lifetime { outlives } => {
            write!(f, "{}", this.0.name)?;
            if !outlives.is_empty() {
                write!(f, ": ")?;
                intersperse(f, " + ", outlives, String::fmt)?;
            }
        }
        rustdoc_types::GenericParamDefKind::Type {
            bounds,
            default,
            is_synthetic,
        } => {
            if *is_synthetic {
                unreachable!(
                    "synthetic generic definitions do not occur in types.\n\
                    This code will need to be updated if used to format generic definitions \
                    (e.g., in functions)."
                )
            }

            write!(f, "{}", this.0.name)?;
            intersperse_with(f, " + ", bounds, |gb| GenericBound(gb, bounds.len() > 1))?;

            if let Some(def) = default {
                write!(f, " = {}", Type(def, false))?;
            }
        }
        rustdoc_types::GenericParamDefKind::Const { type_, default } => {
            write!(f, "const {}: {}", this.0.name, Type(type_, false))?;

            if let Some(def) = default {
                write!(f, " = {def}")?;
            }
        }
    }
    Ok(())
}

display_wrapper!(GenericParamDef, fmt_generic_param_def);

fn fmt_poly_trait(this: &PolyTrait, f: &mut Formatter<'_>) -> Result {
    if !this.0.generic_params.is_empty() {
        write!(f, "for<")?;
        intersperse_with(f, ", ", &this.0.generic_params, GenericParamDef)?;
        write!(f, "> ")?;
    }

    write!(f, "{}", Path(&this.0.trait_, this.1))
}

display_wrapper!(PolyTrait, fmt_poly_trait, bool);

fn fmt_type(this: &Type, f: &mut Formatter<'_>) -> Result {
    match this.0 {
        rustdoc_types::Type::ResolvedPath(path) => {
            assert!(
                !matches!(
                    path.args.as_deref(),
                    Some(rustdoc_types::GenericArgs::Parenthesized { .. })
                ),
                "args for a type (vs a trait) should be bracketed, not parenthesized"
            );
            write!(f, "{}", Path(path, false))
        }
        rustdoc_types::Type::DynTrait(dyn_trait) => {
            if this.1 {
                write!(f, "(")?;
            }

            let num_args = dyn_trait.traits.len() + dyn_trait.lifetime.iter().count();

            write!(f, "dyn ")?;
            intersperse(
                f,
                " + ",
                dyn_trait.traits.iter().enumerate(),
                |(i, pt), f| {
                    // If this is the last bound, it is not followed by a +, and e.g., if it
                    // is Fn() -> &dyn ..., it doesn't need to be wrapped in parentheses.
                    write!(f, "{}", PolyTrait(pt, i + 1 < num_args))
                },
            )?;

            if let Some(lt) = &dyn_trait.lifetime {
                write!(f, " + {lt}")?;
            }

            if this.1 {
                write!(f, ")")?;
            }

            Ok(())
        }
        rustdoc_types::Type::Generic(t) => write!(f, "{t}"),
        rustdoc_types::Type::Primitive(t) => write!(f, "{t}"),
        rustdoc_types::Type::FunctionPointer(fnp) => {
            // order: for<_> const async unsafe fn()
            if !fnp.generic_params.is_empty() {
                write!(f, "for<")?;
                intersperse_with(f, ", ", &fnp.generic_params, GenericParamDef)?;
                write!(f, "> ")?;
            }

            if fnp.header.is_const {
                write!(f, "const ")?;
            }

            if fnp.header.is_async {
                write!(f, "async ")?;
            }

            if fnp.header.is_unsafe {
                write!(f, "unsafe ")?;
            }

            macro_rules! abi_name {
                ($f:expr, $name:literal, $unwind:expr) => {{
                    write!($f, concat!("extern \"", $name))?;

                    if *$unwind {
                        write!(f, "-unwind")?;
                    }

                    write!(f, "\" ")?;
                }};
            }

            match &fnp.header.abi {
                rustdoc_types::Abi::Rust => (),
                rustdoc_types::Abi::C { unwind } => abi_name!(f, "C", unwind),
                rustdoc_types::Abi::Cdecl { unwind } => abi_name!(f, "cdecl", unwind),
                rustdoc_types::Abi::Stdcall { unwind } => abi_name!(f, "stdcall", unwind),
                rustdoc_types::Abi::Fastcall { unwind } => abi_name!(f, "fastcall", unwind),
                rustdoc_types::Abi::Aapcs { unwind } => abi_name!(f, "aapcs", unwind),
                rustdoc_types::Abi::Win64 { unwind } => abi_name!(f, "win64", unwind),
                rustdoc_types::Abi::SysV64 { unwind } => abi_name!(f, "sysv64", unwind),
                rustdoc_types::Abi::System { unwind } => abi_name!(f, "system", unwind),
                rustdoc_types::Abi::Other(other) => write!(f, r#"extern "{other}" "#)?,
            }

            write!(f, "fn(")?;

            enum Arg<'a> {
                Named(&'a str, Type<'a>),
                Dots,
            }

            intersperse(
                f,
                ", ",
                fnp.sig
                    .inputs
                    .iter()
                    .map(|(name, ty)| Arg::Named(name, Type(ty, false)))
                    .chain(fnp.sig.is_c_variadic.then_some(Arg::Dots)),
                |arg, f| match arg {
                    Arg::Named(name, ty) => write!(f, "{name}: {ty}"),
                    Arg::Dots => write!(f, "..."),
                },
            )?;

            write!(f, ")")?;

            if let Some(output) = &fnp.sig.output {
                // If `this` follows + bounds, so does the output, so the output
                // will need to be wrapped in parentheses if `this.1` is true and it
                // is a `dyn/impl Trait`.
                write!(f, " -> {}", Type(output, this.1))?;
            }

            Ok(())
        }
        rustdoc_types::Type::Tuple(tys) => {
            write!(f, "(")?;
            intersperse_with(f, ", ", tys, |x| Type(x, false))?;
            write!(f, ")")
        }
        rustdoc_types::Type::Slice(ty) => write!(f, "[{}]", Type(ty, false)),
        // According to the docs for this variant, `len` is not guaranteed to be the
        // same as the source code.  For example, [u8; 1 + 2] currently becomes [u8; 3]
        rustdoc_types::Type::Array { type_, len } => write!(f, "[{}; {}]", Type(type_, false), len),
        rustdoc_types::Type::ImplTrait(vec) => {
            if this.1 {
                write!(f, "(")?;
            }
            write!(f, "impl ")?;
            intersperse_with(f, " + ", vec, |gb| GenericBound(gb, vec.len() > 1))?;

            if this.1 {
                write!(f, ")")?;
            }

            Ok(())
        }
        rustdoc_types::Type::Infer => write!(f, "_"),
        rustdoc_types::Type::RawPointer { is_mutable, type_ } => {
            // When there are multiple bounds on a `dyn` or `impl` trait raw pointer,
            // it needs to be wrapped in parentheses.
            let force_wrap_parens = match &**type_ {
                rustdoc_types::Type::DynTrait(dyn_trait) => {
                    dyn_trait.traits.len() + dyn_trait.lifetime.iter().count() > 1
                }
                rustdoc_types::Type::ImplTrait(bounds) => bounds.len() > 1,
                _ => false,
            };

            let kind = if *is_mutable { "mut" } else { "const" };
            write!(f, "*{kind} {}", Type(type_, force_wrap_parens || this.1))
        }
        rustdoc_types::Type::BorrowedRef {
            lifetime,
            is_mutable,
            type_,
        } => {
            write!(f, "&")?;
            if let Some(lt) = lifetime {
                write!(f, "{lt} ")?;
            }

            if *is_mutable {
                write!(f, "mut ")?;
            }

            // References to `dyn/impl Trait`s must be wrapped in parentheses if there
            // more than 1 bound.
            let force_wrap_parens = match &**type_ {
                rustdoc_types::Type::DynTrait(dyn_trait) => {
                    dyn_trait.traits.len() + dyn_trait.lifetime.iter().count() > 1
                }
                rustdoc_types::Type::ImplTrait(bounds) => bounds.len() > 1,
                _ => false,
            };

            write!(f, "{}", Type(type_, force_wrap_parens || this.1))
        }
        rustdoc_types::Type::QualifiedPath {
            name,
            args,
            self_type,
            trait_,
        } => {
            if let Some(trait_) = trait_ {
                write!(f, "<{} as {}>", Type(self_type, false), Path(trait_, false))?;
            } else {
                write!(f, "{}", Type(self_type, false))?;
            }

            assert!(
                !matches!(&**args, rustdoc_types::GenericArgs::Parenthesized { .. }),
                "args for a type should be bracketed, not parenthesized"
            );

            write!(f, "::{}{}", name, GenericArgs(args, false))
        }
        rustdoc_types::Type::Pat { .. } => unimplemented!("Type::Pat is unstable"),
    }
}

// The `bool` parameter represents whether this type comes before `+` bounds.
// In cases where the type ends in a `dyn Trait` or `impl Trait`, it will need to
// be wrapped in parentheses.
display_wrapper!(Type, fmt_type, bool);

fn fmt_generic_bound(this: &GenericBound, f: &mut Formatter<'_>) -> Result {
    match &this.0 {
        rustdoc_types::GenericBound::TraitBound {
            trait_,
            generic_params,
            modifier,
        } => {
            if !generic_params.is_empty() {
                write!(f, "for<")?;
                intersperse_with(f, ", ", generic_params, GenericParamDef)?;
                write!(f, "> ")?;
            }

            match modifier {
                rustdoc_types::TraitBoundModifier::Maybe => write!(f, "?")?,
                // TODO: check this/find a good reference.  it's currently unstable
                rustdoc_types::TraitBoundModifier::MaybeConst => write!(f, "~const ")?,
                rustdoc_types::TraitBoundModifier::None => (),
            };

            write!(f, "{}", Path(trait_, this.1))?;

            Ok(())
        }
        rustdoc_types::GenericBound::Outlives(lt) => write!(f, "{lt}"),
        rustdoc_types::GenericBound::Use(vec) => {
            write!(f, "use<")?;
            intersperse(f, ", ", vec, String::fmt)?;
            write!(f, ">")
        }
    }
}

// The `bool` parameter indicates whether the bound is preceded or followed by a
// ` + ` bound.  In this case, for [`rustdoc_types::GenericBound::Parenthesized`],
// the `-> output` may need to be wrapped in parentheses if could contain `+` bounds
// (e.g., for `dyn` or `impl Trait`) to avoid ambiguity.
display_wrapper!(GenericBound, fmt_generic_bound, bool);

fn fmt_constant(this: &Constant, f: &mut Formatter<'_>) -> Result {
    if let Some(val) = &this.0.value {
        write!(f, "{}", val)
    } else {
        // The stringified form is unstable.  For example, `{ 1 + 2 }` currently
        // becomes `{ _ }`.
        write!(f, "{}", this.0.expr)
    }
}

display_wrapper!(Constant, fmt_constant);

fn fmt_generic_args(this: &GenericArgs, f: &mut Formatter<'_>) -> Result {
    match this.0 {
        rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
            if !constraints.is_empty() || !args.is_empty() {
                write!(f, "<")?;
            }

            if !args.is_empty() {
                intersperse(f, ", ", args, |arg, f| match arg {
                    rustdoc_types::GenericArg::Lifetime(lt) => {
                        write!(f, "{lt}")
                    }
                    rustdoc_types::GenericArg::Type(t) => {
                        write!(f, "{}", Type(t, false))
                    }
                    rustdoc_types::GenericArg::Const(constant) => {
                        write!(f, "{}", Constant(constant))
                    }
                    rustdoc_types::GenericArg::Infer => write!(f, "_"),
                })?;
            }

            if !constraints.is_empty() {
                if !args.is_empty() {
                    write!(f, ", ")?;
                }

                intersperse(f, ", ", constraints, |constraint, f| {
                    write!(
                        f,
                        "{}{}",
                        constraint.name,
                        GenericArgs(&constraint.args, false)
                    )?;
                    match &constraint.binding {
                        rustdoc_types::AssocItemConstraintKind::Constraint(c) => {
                            write!(f, ": ")?;
                            intersperse_with(f, " + ", c, |gb| GenericBound(gb, c.len() > 1))?;
                        }
                        rustdoc_types::AssocItemConstraintKind::Equality(e) => {
                            write!(f, " = ")?;
                            match e {
                                rustdoc_types::Term::Type(ty) => write!(f, "{}", Type(ty, false))?,
                                rustdoc_types::Term::Constant(c) => write!(f, "{}", Constant(c))?,
                            }
                        }
                    }
                    Ok(())
                })?;
            }

            if !constraints.is_empty() || !args.is_empty() {
                write!(f, ">")?;
            }
        }
        rustdoc_types::GenericArgs::Parenthesized { inputs, output } => {
            write!(f, "(")?;
            intersperse_with(f, ", ", inputs, |ty| Type(ty, false))?;
            write!(f, ")")?;
            if let Some(output) = output {
                write!(f, " -> {}", Type(output, this.1))?;
            }
        }
    }

    Ok(())
}

display_wrapper!(GenericArgs, fmt_generic_args, bool);

fn fmt_path(this: &Path, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", this.0.name)?;
    if let Some(args) = this.0.args.as_deref() {
        write!(f, "{}", GenericArgs(args, this.1))?;
    }
    Ok(())
}

display_wrapper!(Path, fmt_path, bool);

#[cfg(test)]
mod tests {
    use anyhow::Context as _;
    use maplit::btreemap;
    use rustdoc_types::{Item, ItemEnum, StructKind};
    use trustfall::{Schema, TryIntoStruct as _};

    use crate::{IndexedCrate, RustdocAdapter};

    use super::rust_type_name;

    #[test]
    fn typename() {
        let path = "./localdata/test_data/rust_type_name/rustdoc.json";
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to load rustdoc");
        let crate_ = serde_json::from_str(&content).expect("failed to parse rustdoc");
        let indexed_crate = IndexedCrate::new(&crate_);
        let adapter = RustdocAdapter::new(&indexed_crate, None);

        let query = r#"
            {
                Crate {
                    item {
                        ... on Struct {
                            name @filter(op: "=", value: ["$struct"])

                            field {
                                name @output
                                raw_type {
                                    typename: name @output
                                }
                            }
                        }
                    }
                }
            }
        "#;

        let variables = btreemap! {
            "struct" => "Struct",
        };

        let schema = Schema::parse(include_str!("../rustdoc_schema.graphql"))
            .expect("schema failed to parse");

        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, serde::Deserialize)]
        struct Output {
            name: String,
            typename: String,
        }

        let mut results: Vec<Output> =
            trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
                .expect("failed to run query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.sort_unstable();

        similar_asserts::assert_eq!(
            results,
            vec![
                Output {
                    name: "a".into(),
                    typename: "String".into()
                },
                Output {
                    name: "b".into(),
                    typename: "T".into()
                },
                Output {
                    name: "c".into(),
                    typename: "Option<T>".into()
                },
                Output {
                    name: "d".into(),
                    typename: "<A as MyTrait>::A<'static, ()>".into()
                },
                Output {
                    name: "e".into(),
                    typename: "()".into(),
                },
                Output {
                    name: "f".into(),
                    typename: r#"unsafe extern "C-unwind" fn() -> T"#.into(),
                },
                Output {
                    name: "g".into(),
                    typename: "Box<dyn for<'b> MyTrait2<'b, b'a', B = &'b ()> + Send + 'a>".into(),
                }
            ]
        );
    }

    /// Helper function to run a closure on the root module of the `raw_type_json` test crate.
    fn with_crate_root(f: impl FnOnce(&rustdoc_types::Crate, &rustdoc_types::Module)) {
        let path = "./localdata/test_data/raw_type_json/rustdoc.json";
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
            .expect("failed to load rustdoc");
        let crate_: rustdoc_types::Crate =
            serde_json::from_str(&content).expect("failed to parse rustdoc");

        let module = crate_.index.get(&crate_.root).expect("no root");

        let rustdoc_types::ItemEnum::Module(module) = &module.inner else {
            panic!("root is not a module");
        };

        f(&crate_, module)
    }

    #[test]
    fn raw_type_json() {
        with_crate_root(|crate_, module| {
            let func = module
                .items
                .iter()
                .find_map(|x| {
                    let item = crate_.index.get(x)?;
                    if item.name.as_ref()? == "my_generic_function" {
                        if let rustdoc_types::ItemEnum::Function(func) = &item.inner {
                            return Some(func);
                        }
                    }

                    None
                })
                .expect("couldn't find `my_generic_function`");

            let inputs: Vec<_> = func
                .sig
                .inputs
                .iter()
                .map(|(k, v)| (k, rust_type_name(v)))
                .collect();

            similar_asserts::assert_eq!(
                inputs
                    .iter()
                    .map(|(k, v)| (k.as_str(), v.as_str()))
                    .collect::<Vec<_>>(),
                vec![
                    ("a", "&'a &'static mut *const T"),
                    ("b", "&(dyn Iterator<Item = T> + Unpin + Send)"),
                    ("c", "Constant<25>"),
                    (
                        "d",
                        "impl for<'x> FnMut(&'a unsafe extern \"C\" fn(\
                        _: *const [u8], \
                        _: &'x mut *mut (), \
                        ...) -> std::borrow::Cow<'static, [u8]>\
                        ) -> &'x (dyn std::fmt::Display) + Send + 'static"
                    ),
                    ("e", "<U as GAT<T>>::Type<'a, &'static *const ()>"),
                ]
            );

            let output = func.sig.output.as_ref().expect("expected a return type");
            similar_asserts::assert_eq!(
                rust_type_name(output),
                "impl std::future::Future<Output: Iterator<Item: 'a + Send> + \
                for<'z> FnMut(&'z ()) -> &'z &'a ()>"
            );
        });
    }

    #[test]
    fn type_enum() {
        with_crate_root(|crate_, module| {
            let type_struct = module
                .items
                .iter()
                .find_map(|id| {
                    let item = crate_.index.get(id)?;
                    if item.name.as_ref()? == "TypeEnum" {
                        if let ItemEnum::Struct(s) = &item.inner {
                            return Some(s);
                        }
                    }
                    None
                })
                .expect("did not find TypeEnum struct");

            let StructKind::Plain { fields, .. } = &type_struct.kind else {
                panic!("expected Plain struct");
            };

            let names: Vec<_> = fields
                .iter()
                .map(|id| {
                    let Some(Item {
                        name: Some(name),
                        inner: ItemEnum::StructField(ty),
                        ..
                    }) = crate_.index.get(id)
                    else {
                        panic!("expected StructField");
                    };

                    (name, rust_type_name(ty))
                })
                .collect();

            similar_asserts::assert_eq!(
                names
                    .iter()
                    .map(|(k, v)| (k.as_str(), v.as_str()))
                    .collect::<Vec<_>>(),
                vec![
                    ("resolved_path", "Option<()>"),
                    ("dyn_trait", "Box<dyn std::io::Read>"),
                    ("generic", "T"),
                    ("primitive", "u32"),
                    ("function_pointer", "fn(_: ()) -> i32"),
                    ("tuple", "(u32, (), T)"),
                    ("slice", "Box<[u8]>"),
                    ("array", "[(); 3]"),
                    ("array2", "[(); 1]"),
                    ("array3", "[(); 30]"),
                    ("raw_pointer", "*const u8"),
                    ("borrowed_ref", "&'static str"),
                    (
                        "qualified_path",
                        "<std::str::SplitAsciiWhitespace<'static> as Iterator>::Item"
                    ),
                ]
            )
        });
    }

    #[test]
    fn is_synthetic() {
        with_crate_root(|crate_, module| {
            let func = module
                .items
                .iter()
                .find_map(|x| {
                    let item = crate_.index.get(x)?;
                    if item.name.as_ref()? == "is_synthetic" {
                        if let rustdoc_types::ItemEnum::Function(func) = &item.inner {
                            return Some(func);
                        }
                    }

                    None
                })
                .expect("couldn't find `is_synthetic`");

            let inputs: Vec<_> = func
                .sig
                .inputs
                .iter()
                .map(|(k, v)| (k, rust_type_name(v)))
                .collect();

            similar_asserts::assert_eq!(
                inputs
                    .iter()
                    .map(|(k, v)| (k.as_str(), v.as_str()))
                    .collect::<Vec<_>>(),
                vec![("x", "impl std::any::Any"),]
            );
            similar_asserts::assert_eq!(
                rust_type_name(func.sig.output.as_ref().expect("no output")),
                "impl std::any::Any"
            );
        });
    }

    #[test]
    fn dyn_ambiguity() {
        with_crate_root(|crate_, module| {
            let func = module
                .items
                .iter()
                .find_map(|x| {
                    let item = crate_.index.get(x)?;
                    if item.name.as_ref()? == "dyn_ambiguity" {
                        if let rustdoc_types::ItemEnum::Function(func) = &item.inner {
                            return Some(func);
                        }
                    }

                    None
                })
                .expect("couldn't find fn `dyn_ambiguity`");

            let inputs: Vec<_> = func
                .sig
                .inputs
                .iter()
                .map(|(k, v)| (k, rust_type_name(v)))
                .collect();

            similar_asserts::assert_eq!(
                inputs
                    .iter()
                    .map(|(k, v)| (k.as_str(), v.as_str()))
                    .collect::<Vec<_>>(),
                vec![("a", "&'a (impl Fn() -> *const fn() -> &'a (dyn Iterator<Item = ()> + Unpin) + Send)"),
                    ("b", "Box<dyn Fn() -> *const (dyn Unpin + Fn() -> &'static mut (dyn std::any::Any + Sync)) + Sync>"),
                    ("c", "fn() -> &'a (dyn Send + Fn() -> *const dyn std::any::Any)"),
                    ("no_parens", "impl for<'x> Fn(&'x ()) -> &'x dyn std::fmt::Debug"),
                    ("sanity", "&dyn std::fmt::Display"),
                ]
            );
        });
    }
}
