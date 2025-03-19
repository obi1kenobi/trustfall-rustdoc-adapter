use std::borrow::Cow;

use rustdoc_types::{GenericArgs, Type};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Receiver<'a>(&'a Type);

impl<'a> Receiver<'a> {
    pub(super) fn new(ty: &'a Type) -> Self {
        Self(ty)
    }

    #[inline]
    pub(super) fn by_value(&self) -> bool {
        !matches!(self.0, Type::BorrowedRef { .. })
    }

    #[inline]
    pub(super) fn by_reference(&self) -> bool {
        matches!(
            self.0,
            Type::BorrowedRef {
                is_mutable: false,
                ..
            }
        )
    }

    #[inline]
    pub(super) fn by_mut_reference(&self) -> bool {
        matches!(
            self.0,
            Type::BorrowedRef {
                is_mutable: true,
                ..
            }
        )
    }

    pub(super) fn kind(&self) -> Cow<'_, str> {
        extract_kind_string(self.0)
    }
}

fn extract_kind_string(ty: &Type) -> Cow<'_, str> {
    match ty {
        // For &self and &mut self, we need to extract the inner type
        Type::BorrowedRef { type_, .. } => extract_kind_string(type_),

        // Self is the simplest case - this handles both 'self' and 'mut self'
        Type::Generic(name) if name == "Self" => Cow::Borrowed("Self"),

        // Handle ResolvedPath types like Box<Self>, Pin<&mut Self>, etc.
        Type::ResolvedPath(path) => {
            // Get just the type name without the path
            let name = path.path.split("::").last().unwrap_or(&path.path);

            if let Some(args) = &path.args {
                match args.as_ref() {
                    GenericArgs::AngleBracketed { args, .. } => {
                        let args_str: Vec<Cow<'_, str>> = args
                            .iter()
                            .map(|arg| match arg {
                                rustdoc_types::GenericArg::Type(t) => {
                                    // For Pin<&mut Self>, we need to preserve the &mut
                                    match t {
                                        Type::BorrowedRef {
                                            is_mutable, type_, ..
                                        } => {
                                            let inner = extract_kind_string(type_);
                                            if *is_mutable {
                                                Cow::Owned(format!("&mut {}", inner))
                                            } else {
                                                Cow::Owned(format!("&{}", inner))
                                            }
                                        }
                                        _ => extract_kind_string(t),
                                    }
                                }
                                rustdoc_types::GenericArg::Lifetime(lt) => {
                                    Cow::Borrowed(lt.as_str())
                                }
                                rustdoc_types::GenericArg::Const(c) => {
                                    Cow::Borrowed(c.expr.as_str())
                                }
                                _ => unreachable!("should not encounter infer"),
                            })
                            .collect();

                        let args_joined = args_str
                            .iter()
                            .map(|cow| cow.as_ref())
                            .collect::<Vec<_>>()
                            .join(", ");

                        Cow::Owned(format!("{}<{}>", name, args_joined))
                    }
                    _ => Cow::Borrowed(name),
                }
            } else {
                Cow::Borrowed(name)
            }
        }

        // should not encounter other types
        _ => unreachable!("unsupported type: {:?}", ty),
    }
}
