use std::fmt::{Display, Formatter, Result};

/// Serializes the `rustdoc_types::Type` type as a String containing the name
/// of the type, as close as possible to the original code declaration.
pub(crate) fn rust_type_name(ty: &rustdoc_types::Type) -> String {
    Type(ty).to_string()
}

/// Creates a struct named `$t` that wraps a `rustdoc_types::$t` reference,
/// and implements `Display` on it by calling the given `$formatter` function.
macro_rules! display_wrapper {
    ($vis:vis $t:ident, $formatter:ident) => {
        #[doc = concat!(
            "Wraps a &rustdoc_types::",
            stringify!($t),
            " to implement [`Display`]."
        )]
        #[derive(Debug, Clone, Copy)]
        $vis struct $t<'a>($vis &'a rustdoc_types::$t);

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
                todo!("is_synthetic generic");
            }

            write!(f, "{}", this.0.name)?;
            intersperse_with(f, " + ", bounds, GenericBound)?;

            if let Some(def) = default {
                write!(f, " = {}", Type(def))?;
            }
        }
        rustdoc_types::GenericParamDefKind::Const { type_, default } => {
            write!(f, "const {}: {}", this.0.name, Type(type_))?;

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

    write!(f, "{}", Path(&this.0.trait_))
}

display_wrapper!(PolyTrait, fmt_poly_trait);

fn fmt_type(this: &Type, f: &mut Formatter<'_>) -> Result {
    match this.0 {
        rustdoc_types::Type::ResolvedPath(path) => write!(f, "{}", Path(path)),
        rustdoc_types::Type::DynTrait(dyn_trait) => {
            write!(f, "dyn ")?;
            intersperse_with(f, " + ", &dyn_trait.traits, PolyTrait)?;

            if let Some(lt) = &dyn_trait.lifetime {
                write!(f, " + {lt}")?;
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
                    .map(|(name, ty)| Arg::Named(name, Type(ty)))
                    .chain(fnp.sig.is_c_variadic.then_some(Arg::Dots)),
                |arg, f| match arg {
                    Arg::Named(name, ty) => write!(f, "{name}: {ty}"),
                    Arg::Dots => write!(f, "..."),
                },
            )?;

            write!(f, ")")?;

            if let Some(output) = &fnp.sig.output {
                write!(f, " -> {}", Type(output))?;
            }

            Ok(())
        }
        rustdoc_types::Type::Tuple(tys) => {
            write!(f, "(")?;
            intersperse_with(f, ", ", tys, Type)?;
            write!(f, ")")
        }
        rustdoc_types::Type::Slice(ty) => write!(f, "[{}]", Type(ty)),
        // according to docs, `len` is not guaranteed to roundtrip
        rustdoc_types::Type::Array { type_, len } => write!(f, "[{}; {}]", Type(type_), len),
        rustdoc_types::Type::ImplTrait(vec) => {
            write!(f, "impl ")?;
            intersperse_with(f, " + ", vec, GenericBound)
        }
        rustdoc_types::Type::Infer => write!(f, "_"),
        rustdoc_types::Type::RawPointer { is_mutable, type_ } => {
            let kind = if *is_mutable { "mut" } else { "const" };

            write!(f, "*{kind} {}", Type(type_))
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

            write!(f, "{}", Type(type_))
        }
        rustdoc_types::Type::QualifiedPath {
            name,
            args,
            self_type,
            trait_,
        } => {
            if let Some(trait_) = trait_ {
                write!(f, "<{} as {}>", Type(self_type), Path(trait_))?;
            } else {
                write!(f, "{}", Type(self_type))?;
            }

            write!(f, "::{}{}", name, GenericArgs(args))
        }
        rustdoc_types::Type::Pat { .. } => unimplemented!("Type::Pat is unstable"),
    }
}

display_wrapper!(Type, fmt_type);

fn fmt_generic_bound(this: &GenericBound, f: &mut Formatter<'_>) -> Result {
    match &this.0 {
        rustdoc_types::GenericBound::TraitBound {
            trait_,
            generic_params,
            modifier,
        } => {
            match modifier {
                rustdoc_types::TraitBoundModifier::Maybe => write!(f, "?")?,
                // TODO: check this/find a good reference.  it's currently unstablke
                rustdoc_types::TraitBoundModifier::MaybeConst => write!(f, "~const ")?,
                rustdoc_types::TraitBoundModifier::None => (),
            };

            write!(f, "{}", Path(trait_))?;

            if generic_params.is_empty() {
                write!(f, "<")?;
                intersperse_with(f, ", ", generic_params, GenericParamDef)?;
                write!(f, ">")?;
            }

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

display_wrapper!(GenericBound, fmt_generic_bound);

fn fmt_constant(this: &Constant, f: &mut Formatter<'_>) -> Result {
    if let Some(val) = &this.0.value {
        write!(f, "{}", val)
    } else {
        // TODO: investigate expr docs - stringified is unstable
        write!(f, "{}", this.0.expr)
    }
}

display_wrapper!(Constant, fmt_constant);

fn fmt_generic_args(this: &GenericArgs, f: &mut Formatter<'_>) -> Result {
    match this.0 {
        rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
            if !args.is_empty() {
                write!(f, "<")?;
                intersperse(f, ", ", args, |arg, f| match arg {
                    rustdoc_types::GenericArg::Lifetime(lt) => {
                        write!(f, "{lt}")
                    }
                    rustdoc_types::GenericArg::Type(t) => {
                        write!(f, "{}", Type(t))
                    }
                    rustdoc_types::GenericArg::Const(constant) => {
                        write!(f, "{}", Constant(constant))
                    }
                    rustdoc_types::GenericArg::Infer => write!(f, "_"),
                })?;

                if !constraints.is_empty() {
                    if !args.is_empty() {
                        write!(f, ", ")?;
                    }

                    intersperse(f, ", ", constraints, |constraint, f| {
                        write!(f, "{}{}", constraint.name, GenericArgs(&constraint.args))?;
                        match &constraint.binding {
                            rustdoc_types::AssocItemConstraintKind::Constraint(c) => {
                                write!(f, ": ")?;
                                intersperse_with(f, " + ", c, GenericBound)?;
                            }
                            rustdoc_types::AssocItemConstraintKind::Equality(e) => {
                                write!(f, " = ")?;
                                match e {
                                    rustdoc_types::Term::Type(ty) => write!(f, "{}", Type(ty))?,
                                    rustdoc_types::Term::Constant(c) => {
                                        write!(f, "{}", Constant(c))?
                                    }
                                }
                            }
                        }
                        Ok(())
                    })?;
                }

                write!(f, ">")?;
            }
        }
        rustdoc_types::GenericArgs::Parenthesized { inputs, output } => {
            write!(f, "(")?;
            intersperse_with(f, ", ", inputs, Type)?;
            write!(f, ")")?;
            if let Some(output) = output {
                write!(f, " -> {}", Type(output))?;
            }
        }
    }

    Ok(())
}

display_wrapper!(GenericArgs, fmt_generic_args);

fn fmt_path(this: &Path, f: &mut Formatter<'_>) -> Result {
    write!(f, "{}", this.0.name)?;
    if let Some(args) = this.0.args.as_deref() {
        write!(f, "{}", GenericArgs(args))?;
    }
    Ok(())
}

display_wrapper!(Path, fmt_path);

#[cfg(test)]
mod tests {
    use anyhow::Context as _;
    use maplit::btreemap;
    use trustfall::{Schema, TryIntoStruct as _};

    use crate::{IndexedCrate, RustdocAdapter};

    #[test]
    fn typename() {
        let path = "./localdata/test_data/typename/rustdoc.json";
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
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
}
