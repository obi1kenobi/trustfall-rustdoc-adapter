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

display_wrapper! {
    PolyTrait,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if !self.0.generic_params.is_empty() {
            write!(f, "for<")?;
            intersperse(f, ", ", &self.0.generic_params, |param, f| {
                match &param.kind {
                    rustdoc_types::GenericParamDefKind::Lifetime { outlives } => {
                        write!(f, "{}", param.name)?;
                        if !outlives.is_empty() {
                            write!(f, ": ")?;
                            intersperse(f, " + ", outlives, String::fmt)?;
                        }
                    }
                    rustdoc_types::GenericParamDefKind::Type { bounds, default, is_synthetic } => todo!(),
                    rustdoc_types::GenericParamDefKind::Const { type_, default } => todo!(),
                }
                Ok(())
            })?;
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
                intersperse(f, " + ", &dyn_trait.traits, |trait_, f| PolyTrait(trait_).fmt(f))?;

                if let Some(lt) = &dyn_trait.lifetime {
                    write!(f, " + {lt}")?;
                }

                Ok(())
            }
            rustdoc_types::Type::Generic(t) => write!(f, "{t}"),
            rustdoc_types::Type::Primitive(t) => write!(f, "{t}"),
            rustdoc_types::Type::FunctionPointer(function_pointer) => todo!(),
            rustdoc_types::Type::Tuple(vec) => todo!(),
            rustdoc_types::Type::Slice(_) => todo!(),
            rustdoc_types::Type::Array { type_, len } => todo!(),
            rustdoc_types::Type::Pat {
                type_,
                __pat_unstable_do_not_use,
            } => todo!(),
            rustdoc_types::Type::ImplTrait(vec) => todo!(),
            rustdoc_types::Type::Infer => todo!(),
            rustdoc_types::Type::RawPointer { is_mutable, type_ } => todo!(),
            rustdoc_types::Type::BorrowedRef {
                lifetime,
                is_mutable,
                type_,
            } => todo!(),
            rustdoc_types::Type::QualifiedPath {
                name,
                args,
                self_type,
                trait_,
            } => todo!(),
        }
    }
}

display_wrapper! {
    GenericBound,
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        todo!()
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
                            write!(f, "{lt}")
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
                intersperse(f, ", ", inputs, |ty, f| Type(ty).fmt(f))?;
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
