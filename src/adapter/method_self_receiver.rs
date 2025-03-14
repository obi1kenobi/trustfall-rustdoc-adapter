use rustdoc_types::{GenericArgs, Type};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct MethodSelfReceiver<'a>(&'a Type);

impl<'a> MethodSelfReceiver<'a> {
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

    pub(super) fn kind(&self) -> String {
        extract_kind_string(self.0)
    }
}

fn extract_kind_string(ty: &Type) -> String {
    match ty {
        // For &self and &mut self, we need to extract the inner type
        Type::BorrowedRef { type_, .. } => extract_kind_string(type_),

        // Self is the simplest case - this handles both 'self' and 'mut self'
        Type::Generic(name) if name == "Self" => "Self".to_string(),

        // Handle ResolvedPath types like Box<Self>, Pin<&mut Self>, etc.
        Type::ResolvedPath(path) => {
            // Get just the type name without the path
            let name = path.path.split("::").last().unwrap_or(&path.path);

            if let Some(args) = &path.args {
                match args.as_ref() {
                    GenericArgs::AngleBracketed { args, .. } => {
                        let args_str: Vec<String> = args
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
                                                format!("&mut {}", inner)
                                            } else {
                                                format!("&{}", inner)
                                            }
                                        }
                                        _ => extract_kind_string(t),
                                    }
                                }
                                rustdoc_types::GenericArg::Lifetime(lt) => lt.clone(),
                                rustdoc_types::GenericArg::Const(c) => c.expr.clone(),
                                _ => "?".to_string(),
                            })
                            .collect();
                        format!("{}<{}>", name, args_str.join(", "))
                    }
                    _ => name.to_string(),
                }
            } else {
                name.to_string()
            }
        }

        // should not encounter other types
        _ => unreachable!("unsupported type: {:?}", ty),
    }
}
