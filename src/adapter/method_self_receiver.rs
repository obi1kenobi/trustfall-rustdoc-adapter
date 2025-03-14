use rustdoc_types::{GenericArgs, Type};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub(crate) struct MethodSelfReceiver<'a>(&'a Type);

impl<'a> MethodSelfReceiver<'a> {
    pub fn new(ty: &'a Type) -> Self {
        Self(ty)
    }

    #[inline]
    pub(crate) fn by_value(&self) -> bool {
        !matches!(self.0, Type::BorrowedRef { .. })
    }

    #[inline]
    pub(crate) fn by_reference(&self) -> bool {
        matches!(
            self.0,
            Type::BorrowedRef {
                is_mutable: false,
                ..
            }
        )
    }

    #[inline]
    pub(crate) fn by_mut_reference(&self) -> bool {
        matches!(
            self.0,
            Type::BorrowedRef {
                is_mutable: true,
                ..
            }
        )
    }

    pub(crate) fn kind(&self) -> String {
        extract_kind_string(self.0)
    }
}

fn extract_kind_string(ty: &Type) -> String {
    match ty {
        // Self is the simplest case
        Type::Generic(name) if name == "Self" => "Self".to_string(),

        // Handle BorrowedRef by extracting the inner type
        Type::BorrowedRef { type_, .. } => extract_kind_string(type_),

        // Handle ResolvedPath types like Box<Self>, Pin<&mut Self>, etc.
        Type::ResolvedPath(path) => {
            let name = path.path.split("::").last().unwrap();

            if let Some(args) = &path.args {
                match args.as_ref() {
                    GenericArgs::AngleBracketed { args, .. } => {
                        let args_str: Vec<String> = args
                            .iter()
                            .map(|arg| match arg {
                                rustdoc_types::GenericArg::Type(t) => extract_kind_string(t),
                                rustdoc_types::GenericArg::Lifetime(lt) => lt.clone(),
                                rustdoc_types::GenericArg::Const(c) => c.expr.clone(),
                                _ => unreachable!("infer not supported"),
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

        // For other types, just convert to a debug string
        _ => unreachable!("unsupported type: {:?}", ty),
    }
}
