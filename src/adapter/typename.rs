use std::fmt::{Display, Formatter, Result};

macro_rules! display_wrapper {
    ($vis:vis $t:ident,
    $impl:item) => {
        #[derive(Debug, Clone, Copy)]
        $vis struct $t<'a>($vis &'a rustdoc_types::$t);

        impl Display for $t<'_> {
            $impl
        }
    };
}

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

#[inline]
fn intersperse_with<T, I: IntoIterator<Item = T>, U: Display, F: Fn(T) -> U>(
    fmt: &mut Formatter<'_>,
    delim: &str,
    items: I,
    f: F,
) -> Result {
    intersperse(fmt, delim, items, |x, fmt| f(x).fmt(fmt))
}

display_wrapper! {
    GenericParamDef,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.0.kind {
            rustdoc_types::GenericParamDefKind::Lifetime { outlives } => {
                write!(f, "{}", self.0.name)?;
                if !outlives.is_empty() {
                    write!(f, ": ")?;
                    intersperse(f, " + ", outlives, String::fmt)?;
                }
            }
            rustdoc_types::GenericParamDefKind::Type { bounds, default, is_synthetic } => {
                if *is_synthetic {
                    todo!("is_synthetic generic");
                }

                write!(f, "{}", self.0.name)?;
                intersperse_with(f, " + ", bounds, GenericBound)?;

                if let Some(def) = default {
                    write!(f, " = {}", Type(def))?;
                }
            }
            rustdoc_types::GenericParamDefKind::Const { type_, default } => {
                write!(f, "const {}: {}", self.0.name, Type(type_))?;

                if let Some(def) = default {
                    write!(f, " = {def}")?;
                }
            }
        }
        Ok(())
    }
}

display_wrapper! {
    PolyTrait,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if !self.0.generic_params.is_empty() {
            write!(f, "for<")?;
            intersperse_with(f, ", ", &self.0.generic_params, GenericParamDef)?;
            write!(f, ">")?;
        }

        write!(f, "{}", Path(&self.0.trait_))
    }
}

display_wrapper! {
    pub(crate) Type,
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
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
            rustdoc_types::Type::FunctionPointer(fnp) => todo!(),
            rustdoc_types::Type::Tuple(tys) => {
                write!(f, "(")?;
                intersperse_with(f, ", ", tys, Type)?;
                write!(f, ")")
            },
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
            },
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
}

display_wrapper! {
    GenericBound,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.0 {
            rustdoc_types::GenericBound::TraitBound { trait_, generic_params, modifier } => {
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
            },
            rustdoc_types::GenericBound::Outlives(lt) => write!(f,  "{lt}"),
            rustdoc_types::GenericBound::Use(vec) => {
                write!(f, "use<")?;
                intersperse(f, ", ", vec, String::fmt)?;
                write!(f, ">")
            }
        }
    }
}

display_wrapper! {
    GenericArgs,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
                write!(f, "<")?;
                intersperse(f, ", ", args, |arg, f| {
                    match arg {
                        rustdoc_types::GenericArg::Lifetime(lt) => {
                            // TODO: bounds
                            write!(f, "{lt}")?;
                            Ok(())
                        }
                        rustdoc_types::GenericArg::Type(t) => {
                            // TODO: bounds
                            write!(f, "{}", Type(t))
                        },
                        rustdoc_types::GenericArg::Const(constant) => {
                            // TODO: bounds
                            if let Some(val) = &constant.value {
                                write!(f, "{}", val)
                            } else {
                                // TODO: investigate expr docs - stringified is unstable
                                write!(f, "{}", constant.expr)
                            }
                        }
                        rustdoc_types::GenericArg::Infer => write!(f, "_"),
                    }
                })?;

                write!(f, ">")?;
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
}

display_wrapper! {
    Path,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.0.name)?;
        if let Some(args) = self.0.args.as_deref() {
            write!(f, "{}", GenericArgs(args))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Context as _;
    use maplit::btreemap;
    use rustdoc_types::Crate;
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
                                raw_type {
                                    name @output
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

        #[derive(Debug, PartialOrd, Ord, PartialEq, Eq, serde::Deserialize)]
        struct Output {
            name: String,
        }

        let mut results: Vec<_> =
            trustfall::execute_query(&schema, adapter.into(), query, variables.clone())
                .expect("failed to run query")
                .map(|row| row.try_into_struct().expect("shape mismatch"))
                .collect();
        results.sort_unstable();

        similar_asserts::assert_eq!(vec![Output { name: "Hi".into() }], results);
    }
}
