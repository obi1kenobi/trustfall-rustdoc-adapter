use std::borrow::Cow;

use rustdoc_types::{
    Crate, GenericArg, GenericArgs, GenericParamDef, GenericParamDefKind, Generics, ItemEnum, Path,
    Type,
};

use super::rust_type_name::{needs_parens_before_bounds, rust_type_name};

/// A method's `self` parameter and the context for rendering its receiver kind.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Receiver<'a> {
    /// The full receiver type in the method signature, including any outer reference.
    type_: &'a Type,

    /// The resolved path of the declaring impl's self type, paired with that impl's generics.
    /// For `impl<const N: usize> Reader<N>`, the path is `Reader<N>` and the generics
    /// declare `const N: usize`. [`Self::kind`] uses that declaration to identify `N`
    /// in the receiver, so `Box<Reader<N>>` renders as `Box<Self>`.
    ///
    /// This is `None` for a method declared in a trait or when the impl's self type
    /// is not a [`Type::ResolvedPath`], as with `T` in `impl<T> Read for T`.
    /// Without a path to compare, [`Self::kind`] leaves named types in the receiver explicit.
    owner: Option<(&'a Path, &'a Generics)>,
}

impl<'a> Receiver<'a> {
    pub(super) fn new(type_: &'a Type, parent: &'a ItemEnum) -> Self {
        let owner = if let ItemEnum::Impl(impl_) = parent
            // When rustdoc lists `impl<T> Trait for T` on `Example`, `for_` can name `Example`.
            // Use `blanket_impl`'s declared `T` so a receiver that names `Example` stays explicit.
            && let Type::ResolvedPath(path) = impl_.blanket_impl.as_ref().unwrap_or(&impl_.for_)
        {
            Some((path, &impl_.generics))
        } else {
            None
        };
        Self { type_, owner }
    }

    #[inline]
    pub(super) fn by_value(&self) -> bool {
        !matches!(self.type_, Type::BorrowedRef { .. })
    }

    #[inline]
    pub(super) fn by_reference(&self) -> bool {
        matches!(
            self.type_,
            Type::BorrowedRef {
                is_mutable: false,
                ..
            }
        )
    }

    #[inline]
    pub(super) fn by_mut_reference(&self) -> bool {
        matches!(
            self.type_,
            Type::BorrowedRef {
                is_mutable: true,
                ..
            }
        )
    }

    pub(super) fn kind<'b>(&'b self, crate_: &'b Crate) -> Cow<'b, str> {
        // Up to one outer reference is stripped and reported by the other methods.
        // Any inner references, as well as other indirections, remain part of the receiver kind.
        let ty = match self.type_ {
            Type::BorrowedRef { type_, .. } => type_,
            ty => ty,
        };
        self.extract_kind_string(ty, crate_)
    }

    fn extract_kind_string<'b>(&self, ty: &'b Type, crate_: &'b Crate) -> Cow<'b, str> {
        match ty {
            Type::BorrowedRef {
                is_mutable, type_, ..
            } => {
                let prefix = if *is_mutable { "&mut " } else { "&" };
                Cow::Owned(self.reference_or_pointer_kind(prefix, type_, crate_))
            }
            Type::RawPointer { is_mutable, type_ } => {
                let prefix = if *is_mutable { "*mut " } else { "*const " };
                Cow::Owned(self.reference_or_pointer_kind(prefix, type_, crate_))
            }
            Type::Generic(name) | Type::Primitive(name) => Cow::Borrowed(name),
            Type::ResolvedPath(path) => {
                if let Some((owner, impl_generics)) = self.owner
                    && same_instantiation(
                        path,
                        owner,
                        crate_,
                        ArgumentContext {
                            impl_generics: Some(impl_generics),
                            shared_object_lifetime_default: true,
                        },
                    )
                {
                    return Cow::Borrowed("Self");
                }

                // TODO: Let queries distinguish receiver structs with the same declared name.
                // Changing a method's receiver from `a::Pointer<Self>` to `b::Pointer<Self>`
                // can break callers while leaving its `kind` at `Pointer<Self>`.

                // Look up the declaration by ID so a renamed import of `Arc` still renders as `Arc`.
                // For a type alias the ID identifies the alias, so this lookup keeps its name.
                let name = crate_
                    .index
                    .get(&path.id)
                    .and_then(|item| item.name.as_deref())
                    .or_else(|| crate_.paths.get(&path.id)?.path.last().map(String::as_str))
                    .unwrap_or_else(|| {
                        path.path
                            .rsplit_once("::")
                            .map_or(path.path.as_str(), |(_, name)| name)
                    });

                if let Some(GenericArgs::AngleBracketed { args, .. }) = path.args.as_deref() {
                    let args: Vec<_> = args
                        .iter()
                        .map(|arg| match arg {
                            GenericArg::Type(t) => self.extract_kind_string(t, crate_),
                            GenericArg::Lifetime(lt) => Cow::Borrowed(lt.as_str()),
                            GenericArg::Const(c) => Cow::Borrowed(c.expr.as_str()),
                            GenericArg::Infer => Cow::Borrowed("_"),
                        })
                        .collect();
                    Cow::Owned(format!("{name}<{}>", args.join(", ")))
                } else {
                    Cow::Borrowed(name)
                }
            }
            // Use the regular type formatter to preserve an associated type used as a receiver
            // or a type such as a tuple used as an extra argument of a custom receiver.
            _ => Cow::Owned(rust_type_name(ty)),
        }
    }

    fn reference_or_pointer_kind(&self, prefix: &str, pointee: &Type, crate_: &Crate) -> String {
        let inner = self.extract_kind_string(pointee, crate_);
        if needs_parens_before_bounds(pointee) {
            format!("{prefix}({inner})")
        } else {
            format!("{prefix}{inner}")
        }
    }
}

/// Which names and lifetime defaults the two type arguments can share.
#[derive(Clone, Copy)]
struct ArgumentContext<'a> {
    /// Present when both arguments can refer to the same impl parameters by name.
    impl_generics: Option<&'a Generics>,
    /// Whether the containing types give omitted trait object lifetimes the same default.
    /// Set this for two uses of e.g. `Box<dyn Read>` in the same parameter position, where both
    /// object lifetimes default to `'static`. Leave it unset across positions. For example,
    /// in `Pair<'a, A: 'a, B = A>`, two arguments written `*mut dyn Read` acquire the lifetime
    /// `'a` for `A` and `'static` for `B`.
    shared_object_lifetime_default: bool,
}

/// Returns `true` when `left` and `right` resolve to the same item in `crate_`
/// and their generic arguments can be shown to agree. A `false` result can mean
/// either that the paths differ or that an argument comparison was inconclusive.
///
/// The comparison is symmetric. Initially, `left` comes from the receiver
/// and `right` from the impl's self type. Comparing item IDs lets a renamed import
/// match the original path. A custom receiver can contain `Reader<u16>` in an impl
/// for `Reader<u8>`, so even paths with the same item ID can denote different types.
///
/// Either path can omit trailing arguments if the other supplies their declared defaults.
/// [`ArgumentContext`] records whether names and omitted trait object lifetimes
/// can be compared across the two paths, as described in [`same_type_argument`].
fn same_instantiation(
    left: &Path,
    right: &Path,
    crate_: &Crate,
    context: ArgumentContext<'_>,
) -> bool {
    if left.id != right.id {
        // TODO: Expand type aliases when comparing receiver types. An alias has its own item ID,
        // so we miss a match when one path names the alias and the other names its target.
        return false;
    }

    let (Some(left_args), Some(right_args)) = (
        positional_generic_arguments(left),
        positional_generic_arguments(right),
    ) else {
        // These paths carry more than a list of positional arguments.
        // We do not compare that additional information, so we conservatively skip
        // even two occurrences of `Iterator<Item = u8>`.
        return false;
    };
    let (shorter_args, longer_args) = if left_args.len() <= right_args.len() {
        (left_args, right_args)
    } else {
        (right_args, left_args)
    };

    let (paired_args, trailing_args) = longer_args.split_at(shorter_args.len());
    for (left, right) in shorter_args.iter().zip(paired_args) {
        let same_argument = match (left, right) {
            (GenericArg::Type(left), GenericArg::Type(right)) => {
                same_type_argument(left, right, crate_, context)
            }
            (GenericArg::Lifetime(left), GenericArg::Lifetime(right)) => {
                // We compare names without checking outlives bounds.
                // This means we conservatively return `false` in cases where outlives bounds
                // force differently-named lifetimes to be equal: `'a, 'b where 'a: 'b, 'b: 'a`.
                left == right
                    && match left.as_str() {
                        "'static" => true,

                        // Conservatively return `false` because rustdoc shows the same two
                        // `'_` placeholders whether the inferred lifetimes coincide or differ.
                        "'_" => false,

                        // Equal names match when both paths use the impl's lifetime parameters.
                        //
                        // A lifetime inside a declared default has its own scope. We skip it
                        // even if substituting the path's lifetime argument would give a match.
                        _ => context.impl_generics.is_some(),
                    }
            }
            (GenericArg::Const(left), GenericArg::Const(right)) => {
                // Rustdoc can render different const expressions as `{ _ }`, so we only trust
                // matching records for literals and the impl's own const parameters. We miss
                // repeated expressions and equal literals spelled differently, such as `2` and `0x2`.
                left == right
                    && (left.is_literal || is_impl_const(&left.expr, context.impl_generics))
            }
            // If rustdoc supplies `_`, we cannot tell whether inference chose the other
            // argument. Other pairs here have different kinds at this position.
            _ => false,
        };
        if !same_argument {
            return false;
        }
    }
    if trailing_args.is_empty() {
        return true;
    }

    // Each trailing argument must match the default for its position on the shorter path.
    let Some(declaration) = crate_.index.get(&left.id) else {
        // Without the external declaration we cannot look up its default,
        // so we conservatively miss `HashMap<K, V>` paired with `HashMap<K, V, RandomState>`.
        return false;
    };
    let parameters = match &declaration.inner {
        ItemEnum::Struct(item) => &item.generics.params,
        ItemEnum::Enum(item) => &item.generics.params,
        ItemEnum::Union(item) => &item.generics.params,
        // We only look up defaults on structs, enums, and unions. For a trait declared
        // `trait Convert<T = u8>`, this conservatively skips matching `Convert` with `Convert<u8>`.
        _ => return false,
    };
    // Require a parameter for every trailing argument. Otherwise `zip` could skip
    // the last argument and accept the path without checking its default.
    let Some(trailing_params) = parameters.get(shorter_args.len()..longer_args.len()) else {
        return false;
    };
    trailing_args
        .iter()
        .zip(trailing_params)
        .all(|(arg, param)| match (arg, &param.kind) {
            (
                GenericArg::Type(explicit),
                GenericParamDefKind::Type {
                    default: Some(default),
                    ..
                },
            ) => type_matches_default(
                explicit,
                default,
                parameters,
                longer_args,
                crate_,
                context.impl_generics,
            ),
            (
                GenericArg::Const(explicit),
                GenericParamDefKind::Const {
                    default: Some(default),
                    ..
                },
            ) => {
                // Rustdoc stores the default as text. Requiring the same literal avoids
                // equating names from different declarations, but misses an equal value
                // spelled differently or supplied by an earlier const parameter.
                explicit.is_literal && explicit.expr == *default
            }
            // An inferred `_` could equal the default, but its value is unknown here.
            // Otherwise there is no declared default of the required kind to compare.
            _ => false,
        })
}

/// Compares an explicit type argument with its declared default. Type parameters in `default`
/// belong to the declaration. Look them up in `parameters` and take their values from the
/// longer path's `arguments`, where each earlier argument has already matched.
fn type_matches_default(
    explicit: &Type,
    default: &Type,
    parameters: &[GenericParamDef],
    arguments: &[GenericArg],
    crate_: &Crate,
    impl_generics: Option<&Generics>,
) -> bool {
    if let Type::Generic(name) = default
        && let Some((_, GenericArg::Type(argument))) = parameters
            .iter()
            .zip(arguments)
            .find(|(parameter, _)| parameter.name == *name)
    {
        // For `Batch<A, Row = Vec<A>>` in `impl<A> Batch<Option<A>>`, the row is
        // `Vec<Option<A>>`. Once we reach the supplied `Option<A>`, compare it in the
        // impl's scope so its own `A` does not undergo the same substitution again.
        return same_type_argument(
            explicit,
            argument,
            crate_,
            ArgumentContext {
                impl_generics,
                shared_object_lifetime_default: false,
            },
        );
    }

    // Follow type parameters through positional type arguments, as in `Vec<Vec<A>>`.
    // With a mixed list such as `Buffer<A, 4>`, the fallback can miss a match that
    // requires substituting for `A`.
    if let (Type::ResolvedPath(explicit_path), Type::ResolvedPath(default_path)) =
        (explicit, default)
        && let (Some(explicit_args), Some(default_args)) = (
            positional_generic_arguments(explicit_path),
            positional_generic_arguments(default_path),
        )
        && explicit_args.len() == default_args.len()
        && default_args
            .iter()
            .all(|arg| matches!(arg, GenericArg::Type(_)))
    {
        return explicit_path.id == default_path.id
            && explicit_args
                .iter()
                .zip(default_args)
                .all(|(explicit, default)| {
                    let (GenericArg::Type(explicit), GenericArg::Type(default)) =
                        (explicit, default)
                    else {
                        return false;
                    };
                    type_matches_default(
                        explicit,
                        default,
                        parameters,
                        arguments,
                        crate_,
                        impl_generics,
                    )
                });
    }

    // TODO: Track both declarations when an inner type omits its own arguments.
    // For `Outer<A, B = Inner<A>>` and `Inner<X, Y = X>`, matching `Outer<T>` with
    // `Outer<T, Inner<T, T>>` requires substituting `T` for the outer `A` and the inner `X`.
    // A concrete inner default such as `u8` can still match without the outer substitution.
    same_type_argument(
        explicit,
        default,
        crate_,
        ArgumentContext {
            impl_generics: None,
            shared_object_lifetime_default: false,
        },
    )
}

/// Returns the generic arguments of `path` when a positional list fully describes them.
/// A path with no argument list or an empty `<>` yields `Some(&[])`. For example,
/// `Reader` can still match `Reader<u8>` if `u8` is the declared default.
///
/// Returns `None` for associated item constraints, parenthesized arguments, or
/// return type notation. [`same_instantiation`] then declines the match. Treating
/// these forms as empty would incorrectly equate `Iterator<Item = u8>` with `Iterator<Item = u16>`.
fn positional_generic_arguments(path: &Path) -> Option<&[GenericArg]> {
    match path.args.as_deref() {
        None => Some(&[]),
        Some(GenericArgs::AngleBracketed { args, constraints }) if constraints.is_empty() => {
            Some(args)
        }
        _ => None,
    }
}

fn is_impl_const(name: &str, impl_generics: Option<&Generics>) -> bool {
    impl_generics.is_some_and(|generics| {
        generics.params.iter().any(|param| {
            param.name == name && matches!(param.kind, GenericParamDefKind::Const { .. })
        })
    })
}

/// Compares the types `left` and `right` when deciding whether a path within the receiver
/// denotes `Self`. Their order is immaterial. An inconclusive comparison returns `false`
/// and leaves the path explicit.
///
/// Pass the impl's generics in `context` when both types use its parameter declarations.
/// [`type_matches_default`] does this after substituting a supplied argument for a type
/// parameter. For a comparison against an unsubstituted default it passes `None`
/// because a name in that default still belongs to the type declaration.
///
/// Even inside one impl, different parameter positions can give `dyn Trait` different
/// lifetimes. Set `shared_object_lifetime_default` only when the containing types
/// establish that two omitted lifetimes agree.
fn same_type_argument(
    left: &Type,
    right: &Type,
    crate_: &Crate,
    context: ArgumentContext<'_>,
) -> bool {
    let same = |left: &Type, right: &Type| same_type_argument(left, right, crate_, context);
    match (left, right) {
        (Type::ResolvedPath(left), Type::ResolvedPath(right)) => {
            same_instantiation(left, right, crate_, context)
        }
        // Matching the self type, trait instantiation, and item name identifies the same
        // associated type. An inherent associated type has no trait to compare,
        // so we skip even two identical uses.
        // Two distinct projections, such as `<T as Trait>::First` and `<T as Trait>::Second`,
        // can also both resolve to `u8` and conservatively fail this comparison.
        //
        // The item's own arguments can affect its type too. Without comparing them we skip
        // even two occurrences of `<T as Trait>::Item<'a>` to avoid equating different lifetimes.
        (
            Type::QualifiedPath {
                name: left_name,
                args: None,
                self_type: left_self,
                trait_: Some(left_trait),
            },
            Type::QualifiedPath {
                name: right_name,
                args: None,
                self_type: right_self,
                trait_: Some(right_trait),
            },
        ) => {
            left_name == right_name
                // Require rustdoc to name the trait on both sides. An empty path for `T::Item`
                // then prevents a match with either `T::Item` or `<T as Trait>::Item`.
                && !left_trait.path.is_empty()
                && !right_trait.path.is_empty()
                && same(left_self, right_self)
                && same_instantiation(left_trait, right_trait, crate_, context)
        }
        (Type::Primitive(left), Type::Primitive(right)) => left == right,
        // Equal names identify the same parameter only in a shared scope. Without substituting
        // into a tuple default such as `(A,)` we miss `(T,)` when the impl supplies `T` for `A`.
        // If every type implements `Identity` with `Output = Self`, `T: Identity<Output = U>`
        // forces `T` and `U` to agree. Comparing names alone conservatively misses that match.
        (Type::Generic(left), Type::Generic(right)) => {
            context.impl_generics.is_some() && left == right
        }
        (Type::Tuple(left), Type::Tuple(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same(left, right))
        }
        (Type::Slice(left), Type::Slice(right)) => same(left, right),
        (
            Type::Array {
                type_: left,
                len: left_len,
            },
            Type::Array {
                type_: right,
                len: right_len,
            },
        ) => {
            // Rustdoc can abbreviate different lengths to the same `{ _ }`, so we skip
            // expressions even when both produce the same value. Equal decimal spellings match.
            // For a name such as `N`, require a shared impl parameter. A default has its own
            // `N`, so we skip it even when the impl supplies its `N` as the argument.
            left_len == right_len
                && (left_len.parse::<u128>().is_ok()
                    || is_impl_const(left_len, context.impl_generics))
                && same(left, right)
        }
        (
            Type::RawPointer {
                is_mutable: left_mutable,
                type_: left,
            },
            Type::RawPointer {
                is_mutable: right_mutable,
                type_: right,
            },
        ) => left_mutable == right_mutable && same(left, right),
        // Rustdoc leaves an elided or inferred reference lifetime unidentified. Without
        // applying the elision rules, we skip even two occurrences of `fn(&u8)`.
        (
            Type::BorrowedRef {
                lifetime: Some(left_lifetime),
                is_mutable: left_mutable,
                type_: left,
            },
            Type::BorrowedRef {
                lifetime: Some(right_lifetime),
                is_mutable: right_mutable,
                type_: right,
            },
        ) => {
            // Recognize `'static` across declarations and equal names within the same impl.
            // We conservatively neither follow mutual outlives bounds that equate different names
            // nor substitute into a default whose lifetime could equal the impl's.
            left_lifetime == right_lifetime
                && left_lifetime != "'_"
                && (context.impl_generics.is_some() || left_lifetime == "'static")
                && left_mutable == right_mutable
                && same(left, right)
        }
        (Type::DynTrait(left), Type::DynTrait(right)) => {
            // Compare lifetime bounds as written. Without checking outlives constraints
            // or computing the omitted lifetime, we conservatively miss differently named
            // but equal lifetimes and `Box<dyn Read>` paired with `Box<dyn Read + 'static>`.
            let same_lifetime = left.lifetime == right.lifetime
                && match left.lifetime.as_deref() {
                    Some("'static") => true,

                    // Conservatively reject two explicit `'_` bounds. Even in matching
                    // containing types, inference can give them equal or different lifetimes.
                    Some("'_") => false,

                    // Equal names match in a shared impl scope. Inside a declared default
                    // the name has its own scope, so we can miss a match that needs substitution.
                    Some(_) => context.impl_generics.is_some(),

                    // A copied argument keeps its object lifetime. Writing the argument again
                    // in a different position can give it a different default. Without proof
                    // that the defaults agree, we miss even two omissions that both mean `'static`.
                    None => context.shared_object_lifetime_default,
                };

            // Comparing bounds position by position conservatively misses equivalent objects
            // when an auto trait moves or repeats, as in
            // `dyn Send + Sync` vs `dyn Sync + Send + Send`.
            same_lifetime
                && left.traits.len() == right.traits.len()
                && left.traits.iter().zip(&right.traits).all(|(left, right)| {
                    // A `for<'a>` bound introduces its own lifetime. Without matching those
                    // parameters we skip even two occurrences of `for<'a> Trait<'a>`.
                    left.generic_params.is_empty()
                        && right.generic_params.is_empty()
                        && same_instantiation(&left.trait_, &right.trait_, crate_, context)
                })
        }
        (Type::FunctionPointer(left), Type::FunctionPointer(right)) => {
            // Matching locally bound lifetimes would require pairing the two declarations.
            // We therefore conservatively skip even
            // `for<'a> fn(&'a u8)` paired with `for<'b> fn(&'b u8)`.
            left.generic_params.is_empty()
                && right.generic_params.is_empty()
                && left.header == right.header
                && left.sig.is_c_variadic == right.sig.is_c_variadic
                && left.sig.inputs.len() == right.sig.inputs.len()
                && left
                    .sig
                    .inputs
                    .iter()
                    .zip(&right.sig.inputs)
                    .all(|((_, left), (_, right))| same(left, right))
                && match (&left.sig.output, &right.sig.output) {
                    (None, None) => true,
                    (Some(left), Some(right)) => same(left, right),
                    // With only one explicit return type, we would need to prove it denotes `()`.
                    // This misses `fn() -> Done` paired with `fn()` when `type Done = ();`.
                    _ => false,
                }
        }
        // Different rustdoc representations can denote the same type. Without resolving
        // `T::Item`, for example, we miss a match with `u8` under `T: Trait<Item = u8>`.
        // Type forms we do not compare, such as pattern types, fail even when identical.
        _ => false,
    }
}
