//! Normalized generic signatures for Rust items with item-level generics.
//!
//! The schema exposes this for structs, enums, unions, and traits, formatting
//! only the generic parameter list and bounds. The owner item name is
//! intentionally omitted so re-exported names do not affect the output.
//!
//! The normalization rules are:
//!
//! - Generic parameters keep definition order, but their names are normalized by
//!   namespace: lifetimes become `'a`, `'b`, etc.; types become `T1`, `T2`,
//!   etc.; consts become `C1`, `C2`, etc. Nested binders continue the current
//!   sequence so higher-ranked lifetimes cannot collide with outer lifetimes.
//! - Inline bounds and trait supertrait bounds are lifted into `where`
//!   predicates, then merged with existing rustdoc `where` predicates for the
//!   same subject and binder. Lifetime outlives predicates are merged by
//!   subject too.
//! - Trait supertrait bounds use `Self` as their predicate subject.
//! - Associated item bound constraints are lifted into explicit projection
//!   predicates. Associated equality constraints stay attached to the trait path
//!   because top-level associated type equality predicates are not stable Rust.
//! - Paths are normalized in an implementation-defined manner:
//!   local public items use the first publicly importable path found,
//!   local private items fall back to rustdoc's crate path,
//!   and foreign items use the item's path as given in rustdoc.
//! - Predicates sort by subject kind: generic type subjects, associated item
//!   subjects, other generic-dependent subjects, lifetime predicates, and
//!   finally trivial concrete predicates. Associated item subjects sort by
//!   structural projection depth before their formatted subject text.
//!
//! The formatter is intentionally lazy: it derives the string from the rustdoc
//! item only when the schema property is resolved.

// TODO: rustdoc JSON does not guarantee a stable string representation for
// const expressions. The formatter currently renames a const expression only
// when rustdoc's string is exactly the source name of an in-scope const
// parameter, or uses an evaluated `Constant::value` when rustdoc provides one.
// Add parser-backed normalization if public signatures need to rewrite generic
// const expressions such as `N + 1`.

// TODO: revisit rustdoc type shapes such as pattern types and return type
// notation when they stabilize enough to appear in the item generic signatures
// this module formats.

// TODO: revisit precise-capture `use<>` bounds if they become valid in
// item generic signatures such as struct, enum, union, or trait bounds.

use std::collections::BTreeMap;

use rustdoc_types::{
    AssocItemConstraintKind, GenericArg, GenericBound, GenericParamDef, GenericParamDefKind,
    Generics, Item, ItemEnum, Term, TraitBoundModifier, Type, WherePredicate,
};

use crate::PackageIndex;

pub(crate) fn impl_owner_normalized_generic_signature<'ctx>(
    crate_: &'ctx PackageIndex<'ctx>,
    item: &Item,
) -> String {
    let generics = match &item.inner {
        ItemEnum::Struct(inner) => &inner.generics,
        ItemEnum::Enum(inner) => &inner.generics,
        ItemEnum::Union(inner) => &inner.generics,
        _ => unreachable!("expected ImplOwner item, got {item:?}"),
    };

    SignatureFormatter::new(crate_, generics).format()
}

pub(crate) fn trait_normalized_generic_signature<'ctx>(
    crate_: &'ctx PackageIndex<'ctx>,
    item: &Item,
) -> String {
    let inner = match &item.inner {
        ItemEnum::Trait(inner) => inner,
        _ => unreachable!("expected Trait item, got {item:?}"),
    };

    SignatureFormatter::new_with_self_bounds(crate_, &inner.generics, &inner.bounds).format()
}

/// Formats the normalized generic signature for one item.
///
/// The formatter starts from rustdoc's `Generics`, lifts inline bounds into
/// `where`-style predicates, and delegates path/name normalization to
/// `FormatContext`.
struct SignatureFormatter<'a, 'ctx> {
    generics: &'a Generics,
    self_bounds: &'a [GenericBound],
    context: FormatContext<'ctx>,
}

impl<'a, 'ctx> SignatureFormatter<'a, 'ctx> {
    fn new(crate_: &'ctx PackageIndex<'ctx>, generics: &'a Generics) -> Self {
        Self::new_with_options(crate_, generics, &[], false)
    }

    fn new_with_self_bounds(
        crate_: &'ctx PackageIndex<'ctx>,
        generics: &'a Generics,
        self_bounds: &'a [GenericBound],
    ) -> Self {
        Self::new_with_options(crate_, generics, self_bounds, true)
    }

    fn new_with_options(
        crate_: &'ctx PackageIndex<'ctx>,
        generics: &'a Generics,
        self_bounds: &'a [GenericBound],
        allow_self_type: bool,
    ) -> Self {
        Self {
            generics,
            self_bounds,
            context: FormatContext {
                crate_,
                names: NameMap::new(&generics.params),
                allow_self_type,
            },
        }
    }

    fn format(&self) -> String {
        let params = self
            .generics
            .params
            .iter()
            .inspect(|param| assert_not_synthetic_owner_param(param))
            .map(|param| self.context.format_generic_param(param))
            .collect::<Vec<_>>();

        let mut output = format!("<{}>", params.join(", "));
        let predicates = self.collect_predicates();
        let predicates = self.context.format_where_predicates(&predicates);
        if !predicates.is_empty() {
            output.push_str(" where ");
            output.push_str(&predicates.join(", "));
        }

        output
    }

    /// Collect inline generic-param bounds, trait `Self` bounds, and explicit `where` predicates.
    ///
    /// Later normalization stages merge and sort equivalent predicates; this
    /// pass only converts rustdoc's predicate sources into one representation.
    fn collect_predicates(&self) -> Vec<NormalizedPredicate<'a>> {
        let inline_predicates = self
            .generics
            .params
            .iter()
            .inspect(|param| assert_not_synthetic_owner_param(param))
            .filter_map(|param| match &param.kind {
                GenericParamDefKind::Lifetime { outlives } if !outlives.is_empty() => {
                    Some(NormalizedPredicate::Lifetime {
                        lifetime: param.name.as_str(),
                        outlives,
                    })
                }
                GenericParamDefKind::Type { bounds, .. } if !bounds.is_empty() => {
                    Some(NormalizedPredicate::Bound {
                        subject: PredicateSubjectSource::Generic(param.name.as_str()),
                        bounds,
                        generic_params: &[],
                    })
                }
                GenericParamDefKind::Lifetime { .. }
                | GenericParamDefKind::Type { .. }
                | GenericParamDefKind::Const { .. } => None,
            });

        let self_predicate = (!self.self_bounds.is_empty()).then_some(NormalizedPredicate::Bound {
            subject: PredicateSubjectSource::SelfType,
            bounds: self.self_bounds,
            generic_params: &[],
        });

        let where_predicates =
            self.generics
                .where_predicates
                .iter()
                .map(|predicate| match predicate {
                    WherePredicate::BoundPredicate {
                        type_,
                        bounds,
                        generic_params,
                    } => NormalizedPredicate::Bound {
                        subject: PredicateSubjectSource::Type(type_),
                        bounds,
                        generic_params,
                    },
                    WherePredicate::LifetimePredicate { lifetime, outlives } => {
                        NormalizedPredicate::Lifetime {
                            lifetime: lifetime.as_str(),
                            outlives,
                        }
                    }
                    WherePredicate::EqPredicate { .. } => unreachable!(
                        "top-level associated item equality predicates are not stable Rust"
                    ),
                });

        inline_predicates
            .chain(self_predicate)
            .chain(where_predicates)
            .collect()
    }
}

/// Predicate input before name normalization, bound lifting, merging, and sorting.
enum NormalizedPredicate<'a> {
    Bound {
        subject: PredicateSubjectSource<'a>,
        bounds: &'a [GenericBound],
        generic_params: &'a [GenericParamDef],
    },
    Lifetime {
        lifetime: &'a str,
        outlives: &'a [String],
    },
}

/// Rustdoc source for the subject of a bound predicate.
///
/// Inline bounds such as `T: Clone` provide only the generic parameter name,
/// while explicit `where` predicates provide the full rustdoc `Type`.
enum PredicateSubjectSource<'a> {
    SelfType,
    Generic(&'a str),
    Type(&'a Type),
}

/// Coarse predicate subject buckets used before lexicographic subject sorting.
///
/// The order of variants in this enum dictates the normalized ordering between
/// these groups of predicates.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum PredicateGroup {
    GenericType,
    AssociatedItem,
    GenericDependentType,
    Lifetime,
    Trivial,
}

/// A formatted predicate subject together with the metadata needed to sort it.
///
/// Shallower projections get sorted to earlier positions than deeper projections.
#[derive(Clone)]
struct PredicateSubject {
    group: PredicateGroup,
    projection_depth: usize,
    text: String,
}

/// A higher-ranked binder already normalized into output parameter names.
///
/// Keeping binders structured until final rendering avoids parsing formatted
/// `for<...>` text when lifted predicates need multiple enclosing binders.
#[derive(Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct Binder {
    params: Vec<String>,
}

impl Binder {
    fn from_params(params: impl IntoIterator<Item = String>) -> Self {
        Self {
            params: params.into_iter().collect(),
        }
    }

    fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    /// Return a binder representing this outer scope followed by `inner`.
    ///
    /// The order matters for nested higher-ranked scopes: a lifted predicate
    /// must preserve the enclosing `for<>` params before any params introduced
    /// by bounds nested under that predicate.
    fn append(&self, inner: &Self) -> Self {
        // TODO: This might be a lot of cloning. Revisit when we're looking to optimize.
        if self.is_empty() {
            inner.clone()
        } else if inner.is_empty() {
            self.clone()
        } else {
            let mut params = self.params.clone();
            params.extend(inner.params.iter().cloned());
            Self { params }
        }
    }

    /// Render this binder as the prefix placed before a predicate subject.
    fn render_prefix(&self) -> String {
        if self.is_empty() {
            String::new()
        } else {
            format!("for<{}> ", self.params.join(", "))
        }
    }
}

/// Formatted type text plus the predicate-sorting metadata learned while formatting it.
///
/// The formatter and classifier intentionally share one traversal for predicate
/// subjects. That keeps future support for additional rustdoc type shapes from
/// accidentally updating the displayed text without also updating the
/// signature-generic dependency or projection-depth metadata used for ordering.
struct FormattedType {
    /// The normalized Rust-like spelling of the type.
    text: String,
    /// Whether this type mentions a generic parameter visible in the current signature scope.
    mentions_owner_generic: bool,
    /// Number of associated-projection steps in the subject spine.
    projection_depth: usize,
}

/// Key used to merge `T: A` and `T: B` into `T: A + B`.
///
/// The binder is part of the key because `for<'a> T: Trait<&'a U>` and
/// `T: Trait<U>` are predicates over different subject contexts.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct BoundPredicateKey {
    group: PredicateGroup,
    projection_depth: usize,
    subject: String,
    binder: Binder,
}

impl BoundPredicateKey {
    fn new(subject: &PredicateSubject, binder: Binder) -> Self {
        Self {
            group: subject.group,
            projection_depth: subject.projection_depth,
            subject: subject.text.clone(),
            binder,
        }
    }
}

/// Fully formatted predicate text, plus the data that controls final ordering.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct PredicateOutput {
    group: PredicateGroup,
    projection_depth: usize,
    subject: String,
    text: String,
}

impl PredicateOutput {
    fn new(subject: &PredicateSubject, text: String) -> Self {
        Self {
            group: subject.group,
            projection_depth: subject.projection_depth,
            subject: subject.text.clone(),
            text,
        }
    }
}

/// Coarse bound-component buckets used before lexicographic component sorting.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum BoundComponentGroup {
    Trait,
    Use,
    Lifetime,
}

/// One bound component attached to a predicate subject.
///
/// `single_text` preserves syntax appropriate for a one-bound predicate. When
/// several components are joined with `+`, `multi_text` may add parentheses
/// around forms such as `Fn() -> impl Trait`.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct BoundComponent {
    group: BoundComponentGroup,
    sort_key: String,
    single_text: String,
    multi_text: String,
}

/// Associated-bound predicate lifted out of a trait path.
///
/// The binder stored here is only the binder introduced under the associated
/// bound itself. The enclosing `where`-predicate binder is applied by
/// `PredicateAccumulator::insert_lifted()`, so callers cannot forget it.
struct LiftedBoundPredicate {
    subject: PredicateSubject,
    binder: Binder,
    bounds: Vec<BoundComponent>,
}

/// Collects normalized predicates before rendering the final `where` clause.
///
/// This type owns the normalization invariants for predicates: bounds with the
/// same subject and binder are merged, lifetime outlives predicates are merged,
/// lifted associated-bound predicates inherit their enclosing binder, and final
/// rendering applies the shared sort and dedup rules in one place.
struct PredicateAccumulator {
    bound_predicates: BTreeMap<BoundPredicateKey, Vec<BoundComponent>>,
    lifetime_predicates: BTreeMap<String, Vec<String>>,
}

impl PredicateAccumulator {
    fn new() -> Self {
        Self {
            bound_predicates: BTreeMap::new(),
            lifetime_predicates: BTreeMap::new(),
        }
    }

    fn insert_bound(
        &mut self,
        subject: &PredicateSubject,
        binder: Binder,
        bounds: impl IntoIterator<Item = BoundComponent>,
    ) {
        self.bound_predicates
            .entry(BoundPredicateKey::new(subject, binder))
            .or_default()
            .extend(bounds);
    }

    /// Insert associated-bound predicates lifted from a predicate with `enclosing_binder`.
    fn insert_lifted(
        &mut self,
        enclosing_binder: &Binder,
        predicates: impl IntoIterator<Item = LiftedBoundPredicate>,
    ) {
        for predicate in predicates {
            self.insert_bound(
                &predicate.subject,
                enclosing_binder.append(&predicate.binder),
                predicate.bounds,
            );
        }
    }

    fn insert_lifetime(&mut self, subject: String, outlives: impl IntoIterator<Item = String>) {
        self.lifetime_predicates
            .entry(subject)
            .or_default()
            .extend(outlives);
    }

    /// Render all accumulated predicates after merging, sorting, and deduping them.
    fn finish(self) -> Vec<String> {
        let mut outputs = Vec::new();

        for (subject, mut outlives) in self.lifetime_predicates {
            outlives.sort_unstable();
            outlives.dedup();
            let subject = PredicateSubject {
                group: PredicateGroup::Lifetime,
                projection_depth: 0,
                text: subject,
            };
            outputs.push(PredicateOutput::new(
                &subject,
                format!("{}: {}", subject.text, outlives.join(" + ")),
            ));
        }

        for (key, mut bounds) in self.bound_predicates {
            bounds.sort_unstable_by(|left, right| {
                (&left.group, &left.sort_key).cmp(&(&right.group, &right.sort_key))
            });
            bounds.dedup();

            let multiple_bounds = bounds.len() > 1;
            let bounds = bounds
                .iter()
                .map(|bound| {
                    if multiple_bounds {
                        bound.multi_text.as_str()
                    } else {
                        bound.single_text.as_str()
                    }
                })
                .collect::<Vec<_>>()
                .join(" + ");
            let subject = PredicateSubject {
                group: key.group,
                projection_depth: key.projection_depth,
                text: key.subject,
            };
            outputs.push(PredicateOutput::new(
                &subject,
                format!("{}{}: {bounds}", key.binder.render_prefix(), subject.text),
            ));
        }

        outputs.sort_unstable();
        outputs.dedup();
        outputs
            .into_iter()
            .map(|predicate| predicate.text)
            .collect()
    }
}

/// Per-scope normalized names for lifetimes, type parameters, and const params.
///
/// `with_scope()` shadows existing names when entering an HRTB-style binder, so
/// a nested `'a` prints as the next available normalized lifetime instead of
/// colliding with an outer `'a`. The next-name counters are kept separately from
/// the maps because shadowing replaces map entries without reducing the number
/// of names already allocated.
#[derive(Clone, Debug)]
struct NameMap {
    lifetimes: BTreeMap<String, String>,
    types: BTreeMap<String, String>,
    consts: BTreeMap<String, String>,
    next_lifetime: usize,
    next_type: usize,
    next_const: usize,
}

impl NameMap {
    fn new(params: &[GenericParamDef]) -> Self {
        let mut value = Self {
            lifetimes: BTreeMap::new(),
            types: BTreeMap::new(),
            consts: BTreeMap::new(),
            next_lifetime: 0,
            next_type: 0,
            next_const: 0,
        };
        value.add_scope(params);
        value
    }

    fn with_scope(&self, params: &[GenericParamDef]) -> Self {
        let mut value = self.clone();
        value.add_scope(params);
        value
    }

    fn add_scope(&mut self, params: &[GenericParamDef]) {
        for param in params {
            assert_not_synthetic_owner_param(param);
            match param.kind {
                GenericParamDefKind::Lifetime { .. } => {
                    self.lifetimes.insert(
                        param.name.clone(),
                        format!("'{}", letter_name(self.next_lifetime)),
                    );
                    self.next_lifetime += 1;
                }
                GenericParamDefKind::Type { .. } => {
                    self.types
                        .insert(param.name.clone(), format!("T{}", self.next_type + 1));
                    self.next_type += 1;
                }
                GenericParamDefKind::Const { .. } => {
                    self.consts
                        .insert(param.name.clone(), format!("C{}", self.next_const + 1));
                    self.next_const += 1;
                }
            }
        }
    }

    fn lifetime(&self, name: &str) -> String {
        if name == "'static" || name == "'_" {
            name.to_string()
        } else if let Some(renamed) = self.lifetimes.get(name) {
            renamed.clone()
        } else {
            unreachable!(
                "non-special lifetime `{name}` was not found in the normalized signature scope"
            )
        }
    }

    fn type_name(&self, name: &str) -> String {
        self.types.get(name).cloned().unwrap_or_else(|| {
            unreachable!("type parameter `{name}` was not found in the normalized signature scope")
        })
    }

    fn const_name(&self, name: &str) -> String {
        self.consts.get(name).cloned().unwrap_or_else(|| {
            unreachable!("const parameter `{name}` was not found in the normalized signature scope")
        })
    }

    fn type_or_const_name(&self, name: &str) -> String {
        self.types
            .get(name)
            .or_else(|| self.consts.get(name))
            .cloned()
            .unwrap_or_else(|| {
                unreachable!(
                    "generic parameter `{name}` was not found in the normalized signature scope"
                )
            })
    }

    /// Rename a stringified const expression when rustdoc emitted just a const parameter name.
    ///
    /// Rustdoc does not guarantee that const expression strings have a stable
    /// source representation, and parsing/replacing names inside expressions is
    /// intentionally out of scope for now.
    fn const_expr(&self, expr: &str) -> String {
        self.consts
            .get(expr)
            .cloned()
            .unwrap_or_else(|| expr.to_string())
    }
}

/// Formatting context for one normalized-signature scope.
///
/// A context carries the crate index used for normalized paths and the current
/// normalized generic-name mapping. Entering an HRTB-style binder creates a
/// child context with additional names while preserving the outer counters.
#[derive(Clone)]
struct FormatContext<'ctx> {
    crate_: &'ctx PackageIndex<'ctx>,
    names: NameMap,
    allow_self_type: bool,
}

impl<'ctx> FormatContext<'ctx> {
    fn with_scope(&self, params: &[GenericParamDef]) -> Self {
        Self {
            crate_: self.crate_,
            names: self.names.with_scope(params),
            allow_self_type: self.allow_self_type,
        }
    }

    fn format_generic_param(&self, param: &GenericParamDef) -> String {
        match &param.kind {
            GenericParamDefKind::Lifetime { .. } => self.names.lifetime(&param.name),
            GenericParamDefKind::Type { default, .. } => {
                let mut output = self.names.type_name(&param.name);
                if let Some(default) = default {
                    output.push_str(" = ");
                    output.push_str(&self.format_type(default, false));
                }
                output
            }
            GenericParamDefKind::Const { type_, default } => {
                let mut output = format!(
                    "const {}: {}",
                    self.names.const_name(&param.name),
                    self.format_type(type_, false)
                );
                if let Some(default) = default {
                    output.push_str(" = ");
                    output.push_str(&self.names.const_expr(default));
                }
                output
            }
        }
    }

    fn format_where_predicates(&self, predicates: &[NormalizedPredicate<'_>]) -> Vec<String> {
        let mut accumulator = PredicateAccumulator::new();

        for predicate in predicates {
            match predicate {
                NormalizedPredicate::Bound {
                    subject,
                    bounds,
                    generic_params,
                } => {
                    let context = self.with_scope(generic_params);
                    let binder = context.binder(generic_params);
                    let subject = context.predicate_subject(subject);

                    for bound in *bounds {
                        let (component, lifted_predicates) =
                            context.bound_component_for_subject(&subject, bound);
                        accumulator.insert_bound(
                            &subject,
                            binder.clone(),
                            std::iter::once(component),
                        );
                        accumulator.insert_lifted(&binder, lifted_predicates);
                    }
                }
                NormalizedPredicate::Lifetime { lifetime, outlives } => {
                    accumulator.insert_lifetime(
                        self.names.lifetime(lifetime),
                        outlives.iter().map(|name| self.names.lifetime(name)),
                    );
                }
            }
        }

        accumulator.finish()
    }

    fn predicate_subject(&self, subject: &PredicateSubjectSource<'_>) -> PredicateSubject {
        match subject {
            PredicateSubjectSource::SelfType => PredicateSubject {
                group: PredicateGroup::GenericType,
                projection_depth: 0,
                text: "Self".to_string(),
            },
            PredicateSubjectSource::Generic(name) => PredicateSubject {
                group: PredicateGroup::GenericType,
                projection_depth: 0,
                text: self.names.type_name(name),
            },
            PredicateSubjectSource::Type(type_) => {
                let formatted = self.format_type_info(type_, false);
                let group = match type_ {
                    Type::Generic(_) => PredicateGroup::GenericType,
                    Type::QualifiedPath { .. } => PredicateGroup::AssociatedItem,
                    _ if formatted.mentions_owner_generic => PredicateGroup::GenericDependentType,
                    _ => PredicateGroup::Trivial,
                };
                let projection_depth = if group == PredicateGroup::AssociatedItem {
                    formatted.projection_depth
                } else {
                    0
                };
                PredicateSubject {
                    group,
                    projection_depth,
                    text: formatted.text,
                }
            }
        }
    }

    fn bound_component_for_subject(
        &self,
        subject: &PredicateSubject,
        bound: &GenericBound,
    ) -> (BoundComponent, Vec<LiftedBoundPredicate>) {
        let GenericBound::TraitBound {
            trait_,
            generic_params,
            modifier,
        } = bound
        else {
            return (self.bound_component(bound), Vec::new());
        };

        let (single_text, lifted_predicates) =
            self.format_trait_bound_for_subject(subject, trait_, generic_params, modifier, false);
        let (multi_text, _) =
            self.format_trait_bound_for_subject(subject, trait_, generic_params, modifier, true);

        (
            BoundComponent {
                group: BoundComponentGroup::Trait,
                sort_key: single_text.clone(),
                single_text,
                multi_text,
            },
            lifted_predicates,
        )
    }

    fn format_trait_bound_for_subject(
        &self,
        subject: &PredicateSubject,
        trait_: &rustdoc_types::Path,
        generic_params: &[GenericParamDef],
        modifier: &TraitBoundModifier,
        wrap_args_before_bounds: bool,
    ) -> (String, Vec<LiftedBoundPredicate>) {
        let context = self.with_scope(generic_params);
        let binder = context.binder(generic_params);
        let mut output = binder.render_prefix();
        match modifier {
            TraitBoundModifier::None => {}
            TraitBoundModifier::Maybe => output.push('?'),
            TraitBoundModifier::MaybeConst => output.push_str("~const "),
        }

        let trait_path =
            context.trait_path_with_associated_bounds_lifted(trait_, wrap_args_before_bounds);
        output.push_str(&trait_path.bound_text);

        let lifted_predicates = trait_path
            .constraints
            .into_iter()
            .flat_map(|constraint| {
                context.lift_associated_constraint(
                    subject,
                    &trait_path.projection_text,
                    &binder,
                    constraint,
                )
            })
            .collect();

        (output, lifted_predicates)
    }

    fn lift_associated_constraint(
        &self,
        base_subject: &PredicateSubject,
        trait_path: &str,
        binder: &Binder,
        constraint: &rustdoc_types::AssocItemConstraint,
    ) -> Vec<LiftedBoundPredicate> {
        let mut subject = PredicateSubject {
            group: PredicateGroup::AssociatedItem,
            projection_depth: base_subject.projection_depth + 1,
            text: format!(
                "<{} as {trait_path}>::{}",
                base_subject.text, constraint.name
            ),
        };
        if let Some(args) = constraint.args.as_deref() {
            subject
                .text
                .push_str(&self.format_generic_args(args, false));
        }

        match &constraint.binding {
            AssocItemConstraintKind::Equality(_) => {
                unreachable!("associated item equality constraints are retained in trait bounds")
            }
            AssocItemConstraintKind::Constraint(bounds) => bounds
                .iter()
                .flat_map(|bound| {
                    let (component, mut nested_predicates) =
                        self.bound_component_for_subject(&subject, bound);
                    prefix_lifted_binders(binder, &mut nested_predicates);
                    let mut predicates = vec![LiftedBoundPredicate {
                        subject: subject.clone(),
                        binder: binder.clone(),
                        bounds: vec![component],
                    }];
                    predicates.append(&mut nested_predicates);
                    predicates
                })
                .collect(),
        }
    }

    fn bound_component(&self, bound: &GenericBound) -> BoundComponent {
        let group = match bound {
            GenericBound::TraitBound { .. } => BoundComponentGroup::Trait,
            GenericBound::Use(_) => BoundComponentGroup::Use,
            GenericBound::Outlives(_) => BoundComponentGroup::Lifetime,
        };
        let single_text = self.format_generic_bound(bound, false);
        let multi_text = self.format_generic_bound(bound, true);
        BoundComponent {
            group,
            sort_key: single_text.clone(),
            single_text,
            multi_text,
        }
    }

    fn binder(&self, params: &[GenericParamDef]) -> Binder {
        Binder::from_params(
            params
                .iter()
                .inspect(|param| assert_not_synthetic_owner_param(param))
                .map(|param| self.format_generic_param(param)),
        )
    }

    fn format_bounds(&self, bounds: &[GenericBound]) -> String {
        let mut bounds = bounds
            .iter()
            .map(|bound| self.bound_component(bound))
            .collect::<Vec<_>>();
        bounds.sort_unstable_by(|left, right| {
            (&left.group, &left.sort_key).cmp(&(&right.group, &right.sort_key))
        });

        let multiple_bounds = bounds.len() > 1;
        bounds
            .iter()
            .map(|bound| {
                if multiple_bounds {
                    bound.multi_text.as_str()
                } else {
                    bound.single_text.as_str()
                }
            })
            .collect::<Vec<_>>()
            .join(" + ")
    }

    fn format_generic_bound(
        &self,
        bound: &GenericBound,
        wrap_before_or_after_bounds: bool,
    ) -> String {
        match bound {
            GenericBound::TraitBound {
                trait_,
                generic_params,
                modifier,
            } => {
                let context = self.with_scope(generic_params);
                let mut output = context.binder(generic_params).render_prefix();
                match modifier {
                    TraitBoundModifier::None => {}
                    TraitBoundModifier::Maybe => output.push('?'),
                    TraitBoundModifier::MaybeConst => output.push_str("~const "),
                }
                output.push_str(&context.format_path(trait_, wrap_before_or_after_bounds));
                output
            }
            GenericBound::Outlives(lifetime) => self.names.lifetime(lifetime),
            GenericBound::Use(args) => {
                let args = args
                    .iter()
                    .map(|arg| match arg {
                        rustdoc_types::PreciseCapturingArg::Lifetime(lifetime) => {
                            self.names.lifetime(lifetime)
                        }
                        rustdoc_types::PreciseCapturingArg::Param(param) => {
                            self.names.type_or_const_name(param)
                        }
                    })
                    .collect::<Vec<_>>();
                format!("use<{}>", args.join(", "))
            }
        }
    }

    fn format_type(&self, type_: &Type, wrap_before_bounds: bool) -> String {
        self.format_type_info(type_, wrap_before_bounds).text
    }

    /// Format a type and collect predicate-subject metadata in the same traversal.
    ///
    /// Most type formatting call sites only need `FormattedType::text`, but
    /// predicate subjects also need to know whether the type depends on owner
    /// generics and how deeply associated projections are nested.
    fn format_type_info(&self, type_: &Type, wrap_before_bounds: bool) -> FormattedType {
        match type_ {
            Type::ResolvedPath(path) => FormattedType {
                text: self.format_path(path, false),
                mentions_owner_generic: self.path_mentions_generic(path),
                projection_depth: 0,
            },
            Type::DynTrait(dyn_trait) => {
                let mut output = String::new();
                if wrap_before_bounds {
                    output.push('(');
                }

                let wrap_trait_bounds =
                    dyn_trait.traits.len() + usize::from(dyn_trait.lifetime.is_some()) > 1;
                let mentions_owner_generic = dyn_trait.traits.iter().any(|poly_trait| {
                    poly_trait
                        .generic_params
                        .iter()
                        .any(|param| self.param_mentions_existing_generic(param))
                        || self.path_mentions_generic(&poly_trait.trait_)
                }) || dyn_trait
                    .lifetime
                    .as_ref()
                    .is_some_and(|lifetime| self.lifetime_mentions_generic(lifetime));
                let mut parts = dyn_trait
                    .traits
                    .iter()
                    .map(|poly_trait| self.format_poly_trait(poly_trait, wrap_trait_bounds))
                    .collect::<Vec<_>>();
                parts.sort_unstable();
                if let Some(lifetime) = &dyn_trait.lifetime {
                    parts.push(self.names.lifetime(lifetime));
                }

                output.push_str("dyn ");
                output.push_str(&parts.join(" + "));
                if wrap_before_bounds {
                    output.push(')');
                }
                FormattedType {
                    text: output,
                    mentions_owner_generic,
                    projection_depth: 0,
                }
            }
            Type::Generic(name) => FormattedType {
                text: if self.allow_self_type && name == "Self" {
                    "Self".to_string()
                } else {
                    self.names.type_name(name)
                },
                mentions_owner_generic: self.names.types.contains_key(name)
                    || (self.allow_self_type && name == "Self"),
                projection_depth: 0,
            },
            Type::Primitive(name) => FormattedType {
                text: name.clone(),
                mentions_owner_generic: false,
                projection_depth: 0,
            },
            Type::FunctionPointer(pointer) => {
                let context = self.with_scope(&pointer.generic_params);
                let mut output = context.binder(&pointer.generic_params).render_prefix();
                output.push_str(&format_function_header(&pointer.header));
                output.push_str("fn");
                output
                    .push_str(&context.format_function_signature(&pointer.sig, wrap_before_bounds));
                FormattedType {
                    text: output,
                    mentions_owner_generic: self.function_pointer_mentions_generic(pointer),
                    projection_depth: 0,
                }
            }
            Type::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|type_| self.format_type_info(type_, false))
                    .collect::<Vec<_>>();
                let mentions_owner_generic = types.iter().any(|type_| type_.mentions_owner_generic);
                let formatted = match types.as_slice() {
                    [] => "()".to_string(),
                    [type_] => format!("({},)", type_.text),
                    _ => format!(
                        "({})",
                        types
                            .iter()
                            .map(|type_| type_.text.as_str())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                };
                FormattedType {
                    text: formatted,
                    mentions_owner_generic,
                    projection_depth: 0,
                }
            }
            Type::Slice(type_) => {
                let type_ = self.format_type_info(type_, false);
                FormattedType {
                    text: format!("[{}]", type_.text),
                    mentions_owner_generic: type_.mentions_owner_generic,
                    projection_depth: 0,
                }
            }
            Type::Array { type_, len } => {
                let type_ = self.format_type_info(type_, false);
                FormattedType {
                    text: format!("[{}; {}]", type_.text, self.names.const_expr(len)),
                    mentions_owner_generic: type_.mentions_owner_generic
                        || self.const_expr_mentions_generic(len),
                    projection_depth: 0,
                }
            }
            Type::Pat { .. } => unimplemented!("Type::Pat is unstable"),
            Type::ImplTrait(bounds) => {
                let mut output = String::new();
                if wrap_before_bounds {
                    output.push('(');
                }
                output.push_str("impl ");
                output.push_str(&self.format_bounds(bounds));
                if wrap_before_bounds {
                    output.push(')');
                }
                FormattedType {
                    text: output,
                    mentions_owner_generic: bounds
                        .iter()
                        .any(|bound| self.generic_bound_mentions_generic(bound)),
                    projection_depth: 0,
                }
            }
            Type::Infer => FormattedType {
                text: "_".to_string(),
                mentions_owner_generic: false,
                projection_depth: 0,
            },
            Type::RawPointer { is_mutable, type_ } => {
                let kind = if *is_mutable { "mut" } else { "const" };
                let force_wrap_parens = needs_parens_before_bounds(type_);
                let type_ = self.format_type_info(type_, wrap_before_bounds || force_wrap_parens);
                FormattedType {
                    text: format!("*{kind} {}", type_.text),
                    mentions_owner_generic: type_.mentions_owner_generic,
                    projection_depth: 0,
                }
            }
            Type::BorrowedRef {
                lifetime,
                is_mutable,
                type_,
            } => {
                let mut output = String::from("&");
                if let Some(lifetime) = lifetime {
                    output.push_str(&self.names.lifetime(lifetime));
                    output.push(' ');
                }
                if *is_mutable {
                    output.push_str("mut ");
                }
                let type_ = self.format_type_info(
                    type_,
                    wrap_before_bounds || needs_parens_before_bounds(type_),
                );
                output.push_str(&type_.text);
                FormattedType {
                    text: output,
                    mentions_owner_generic: lifetime
                        .as_ref()
                        .is_some_and(|lifetime| self.lifetime_mentions_generic(lifetime))
                        || type_.mentions_owner_generic,
                    projection_depth: 0,
                }
            }
            Type::QualifiedPath {
                name,
                args,
                self_type,
                trait_,
            } => {
                let self_type = self.format_type_info(self_type, false);
                let mut output = String::new();
                let trait_mentions_generic = trait_
                    .as_ref()
                    .is_some_and(|trait_| self.path_mentions_generic(trait_));
                if let Some(trait_) = trait_ {
                    if trait_.path.is_empty() {
                        output.push_str(&self_type.text);
                    } else {
                        output.push('<');
                        output.push_str(&self_type.text);
                        output.push_str(" as ");
                        output.push_str(&self.format_path(trait_, false));
                        output.push('>');
                    }
                } else {
                    output.push_str(&self_type.text);
                }

                output.push_str("::");
                output.push_str(name);
                let args_mention_generic = args
                    .as_deref()
                    .is_some_and(|args| self.generic_args_mention_generic(args));
                if let Some(args) = args.as_deref() {
                    output.push_str(&self.format_generic_args(args, false));
                }
                FormattedType {
                    text: output,
                    mentions_owner_generic: self_type.mentions_owner_generic
                        || trait_mentions_generic
                        || args_mention_generic,
                    projection_depth: self_type.projection_depth + 1,
                }
            }
        }
    }

    fn format_poly_trait(
        &self,
        poly_trait: &rustdoc_types::PolyTrait,
        wrap_before_bounds: bool,
    ) -> String {
        let context = self.with_scope(&poly_trait.generic_params);
        let mut output = context.binder(&poly_trait.generic_params).render_prefix();
        output.push_str(&context.format_path(&poly_trait.trait_, wrap_before_bounds));
        output
    }

    fn format_generic_args(
        &self,
        args: &rustdoc_types::GenericArgs,
        wrap_output_before_bounds: bool,
    ) -> String {
        match args {
            rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
                let parts = self.format_angle_args(args, constraints.iter());
                if parts.is_empty() {
                    String::new()
                } else {
                    format!("<{}>", parts.join(", "))
                }
            }
            rustdoc_types::GenericArgs::Parenthesized { inputs, output } => {
                let inputs = inputs
                    .iter()
                    .map(|type_| self.format_type(type_, false))
                    .collect::<Vec<_>>();
                let mut formatted = format!("({})", inputs.join(", "));
                if let Some(output) = output {
                    formatted.push_str(" -> ");
                    formatted.push_str(&self.format_type(output, wrap_output_before_bounds));
                }
                formatted
            }
            rustdoc_types::GenericArgs::ReturnTypeNotation => "(..)".to_string(),
        }
    }

    /// Format angle-bracketed generic arguments and the selected associated constraints.
    ///
    /// Callers choose which constraints belong in the returned list. This is
    /// used when associated-bound constraints are lifted out of a trait path,
    /// but equality constraints must remain attached to the path.
    fn format_angle_args<'a>(
        &self,
        args: &'a [GenericArg],
        constraints: impl Iterator<Item = &'a rustdoc_types::AssocItemConstraint>,
    ) -> Vec<String> {
        let mut parts = args
            .iter()
            .map(|arg| match arg {
                GenericArg::Lifetime(lifetime) => self.names.lifetime(lifetime),
                GenericArg::Type(type_) => self.format_type(type_, false),
                GenericArg::Const(constant) => self.format_constant(constant),
                GenericArg::Infer => "_".to_string(),
            })
            .collect::<Vec<_>>();
        let mut constraints = constraints
            .map(|constraint| self.format_associated_constraint(constraint))
            .collect::<Vec<_>>();
        constraints.sort_unstable();
        parts.extend(constraints);
        parts
    }

    fn format_associated_constraint(
        &self,
        constraint: &rustdoc_types::AssocItemConstraint,
    ) -> String {
        let mut output = constraint.name.clone();
        if let Some(args) = constraint.args.as_deref() {
            output.push_str(&self.format_generic_args(args, false));
        }

        match &constraint.binding {
            AssocItemConstraintKind::Equality(term) => {
                output.push_str(" = ");
                output.push_str(&self.format_term(term));
            }
            AssocItemConstraintKind::Constraint(bounds) => {
                output.push_str(": ");
                output.push_str(&self.format_bounds(bounds));
            }
        }

        output
    }

    /// Format a trait path while extracting associated-bound constraints.
    ///
    /// The returned `bound_text` is the path used in the original predicate.
    /// `projection_text` is the same path without associated constraints, so it
    /// can be embedded in lifted projection subjects.
    fn trait_path_with_associated_bounds_lifted<'a>(
        &self,
        path: &'a rustdoc_types::Path,
        wrap_args_before_bounds: bool,
    ) -> TraitPathFormat<'a> {
        let mut bound_text = self.normalized_path(path);
        let mut projection_text = bound_text.clone();
        let mut constraints = Vec::new();

        if let Some(args) = path.args.as_deref() {
            match args {
                rustdoc_types::GenericArgs::AngleBracketed {
                    args,
                    constraints: assoc_constraints,
                } => {
                    let equality_constraints = assoc_constraints.iter().filter(|constraint| {
                        matches!(constraint.binding, AssocItemConstraintKind::Equality(_))
                    });
                    let bound_parts = self.format_angle_args(args, equality_constraints);
                    if !bound_parts.is_empty() {
                        bound_text.push_str(&format!("<{}>", bound_parts.join(", ")));
                    }

                    // Projection subjects need only the real generic arguments.
                    // Equality constraints stay on `bound_text`; bound constraints
                    // are lifted out below into separate projection predicates.
                    let projection_parts = self.format_angle_args(args, [].into_iter());
                    if !projection_parts.is_empty() {
                        projection_text.push_str(&format!("<{}>", projection_parts.join(", ")));
                    }

                    constraints.extend(assoc_constraints.iter().filter(|constraint| {
                        matches!(constraint.binding, AssocItemConstraintKind::Constraint(_))
                    }));
                }
                rustdoc_types::GenericArgs::Parenthesized { .. }
                | rustdoc_types::GenericArgs::ReturnTypeNotation => {
                    let formatted = self.format_generic_args(args, wrap_args_before_bounds);
                    bound_text.push_str(&formatted);
                    projection_text.push_str(&formatted);
                }
            }
        }

        TraitPathFormat {
            bound_text,
            projection_text,
            constraints,
        }
    }

    fn format_path(&self, path: &rustdoc_types::Path, wrap_args_before_bounds: bool) -> String {
        let mut output = self.normalized_path(path);
        if let Some(args) = path.args.as_deref() {
            output.push_str(&self.format_generic_args(args, wrap_args_before_bounds));
        }
        output
    }

    /// Return the normalized absolute path used in normalized signatures.
    ///
    /// Local public (importable) items prefer their first importable path,
    /// local private items fall back to rustdoc's crate path,
    /// and external items use rustdoc's recorded path.
    fn normalized_path(&self, path: &rustdoc_types::Path) -> String {
        if self.crate_.own_crate.inner.index.contains_key(&path.id) {
            if let Some(importable_path) = self
                .crate_
                .own_crate
                .first_publicly_importable_name(&path.id)
            {
                return absolute_path(&importable_path.path.components);
            }
        }

        if let Some(summary) = self.crate_.own_crate.inner.paths.get(&path.id) {
            return absolute_path(&summary.path);
        }

        assert!(!path.path.is_empty());
        if path.path.starts_with("::") {
            path.path.clone()
        } else {
            format!("::{}", path.path)
        }
    }

    fn format_term(&self, term: &Term) -> String {
        match term {
            Term::Type(type_) => self.format_type(type_, false),
            Term::Constant(constant) => self.format_constant(constant),
        }
    }

    fn format_constant(&self, constant: &rustdoc_types::Constant) -> String {
        constant
            .value
            .clone()
            .unwrap_or_else(|| self.names.const_expr(&constant.expr))
    }

    fn format_function_signature(
        &self,
        sig: &rustdoc_types::FunctionSignature,
        wrap_output_before_bounds: bool,
    ) -> String {
        let inputs = sig
            .inputs
            .iter()
            .map(|(_, type_)| self.format_type(type_, false))
            .chain(sig.is_c_variadic.then(|| "...".to_string()))
            .collect::<Vec<_>>();
        let mut output = format!("({})", inputs.join(", "));
        if let Some(output_type) = &sig.output {
            output.push_str(" -> ");
            output.push_str(&self.format_type(output_type, wrap_output_before_bounds));
        }
        output
    }

    fn type_mentions_generic(&self, type_: &Type) -> bool {
        match type_ {
            Type::ResolvedPath(path) => self.path_mentions_generic(path),
            Type::DynTrait(dyn_trait) => {
                dyn_trait
                    .traits
                    .iter()
                    .any(|poly_trait| self.path_mentions_generic(&poly_trait.trait_))
                    || dyn_trait
                        .lifetime
                        .as_ref()
                        .is_some_and(|lifetime| self.lifetime_mentions_generic(lifetime))
            }
            Type::Generic(name) => {
                self.names.types.contains_key(name) || (self.allow_self_type && name == "Self")
            }
            Type::Primitive(_) | Type::Infer => false,
            Type::FunctionPointer(pointer) => self.function_pointer_mentions_generic(pointer),
            Type::Tuple(types) => types.iter().any(|type_| self.type_mentions_generic(type_)),
            Type::Slice(type_) => self.type_mentions_generic(type_),
            Type::Array { type_, len } => {
                self.type_mentions_generic(type_) || self.const_expr_mentions_generic(len)
            }
            Type::Pat { .. } => false,
            Type::ImplTrait(bounds) => bounds
                .iter()
                .any(|bound| self.generic_bound_mentions_generic(bound)),
            Type::RawPointer { type_, .. } => self.type_mentions_generic(type_),
            Type::BorrowedRef {
                lifetime, type_, ..
            } => {
                lifetime
                    .as_ref()
                    .is_some_and(|lifetime| self.lifetime_mentions_generic(lifetime))
                    || self.type_mentions_generic(type_)
            }
            Type::QualifiedPath {
                args,
                self_type,
                trait_,
                ..
            } => {
                self.type_mentions_generic(self_type)
                    || trait_
                        .as_ref()
                        .is_some_and(|trait_| self.path_mentions_generic(trait_))
                    || args
                        .as_deref()
                        .is_some_and(|args| self.generic_args_mention_generic(args))
            }
        }
    }

    fn function_pointer_mentions_generic(&self, pointer: &rustdoc_types::FunctionPointer) -> bool {
        pointer
            .generic_params
            .iter()
            .any(|param| self.param_mentions_existing_generic(param))
            || pointer
                .sig
                .inputs
                .iter()
                .any(|(_, type_)| self.type_mentions_generic(type_))
            || pointer
                .sig
                .output
                .as_ref()
                .is_some_and(|type_| self.type_mentions_generic(type_))
    }

    fn path_mentions_generic(&self, path: &rustdoc_types::Path) -> bool {
        path.args
            .as_deref()
            .is_some_and(|args| self.generic_args_mention_generic(args))
    }

    fn generic_args_mention_generic(&self, args: &rustdoc_types::GenericArgs) -> bool {
        match args {
            rustdoc_types::GenericArgs::AngleBracketed { args, constraints } => {
                args.iter()
                    .any(|arg| self.generic_arg_mentions_generic(arg))
                    || constraints.iter().any(|constraint| {
                        constraint
                            .args
                            .as_deref()
                            .is_some_and(|args| self.generic_args_mention_generic(args))
                            || match &constraint.binding {
                                AssocItemConstraintKind::Equality(term) => {
                                    self.term_mentions_generic(term)
                                }
                                AssocItemConstraintKind::Constraint(bounds) => bounds
                                    .iter()
                                    .any(|bound| self.generic_bound_mentions_generic(bound)),
                            }
                    })
            }
            rustdoc_types::GenericArgs::Parenthesized { inputs, output } => {
                inputs.iter().any(|type_| self.type_mentions_generic(type_))
                    || output
                        .as_ref()
                        .is_some_and(|type_| self.type_mentions_generic(type_))
            }
            rustdoc_types::GenericArgs::ReturnTypeNotation => false,
        }
    }

    fn generic_arg_mentions_generic(&self, arg: &GenericArg) -> bool {
        match arg {
            GenericArg::Lifetime(lifetime) => self.lifetime_mentions_generic(lifetime),
            GenericArg::Type(type_) => self.type_mentions_generic(type_),
            GenericArg::Const(constant) => self.constant_mentions_generic(constant),
            GenericArg::Infer => false,
        }
    }

    fn generic_bound_mentions_generic(&self, bound: &GenericBound) -> bool {
        match bound {
            GenericBound::TraitBound {
                trait_,
                generic_params,
                ..
            } => {
                generic_params
                    .iter()
                    .any(|param| self.param_mentions_existing_generic(param))
                    || self.path_mentions_generic(trait_)
            }
            GenericBound::Outlives(lifetime) => self.lifetime_mentions_generic(lifetime),
            GenericBound::Use(args) => args.iter().any(|arg| match arg {
                rustdoc_types::PreciseCapturingArg::Lifetime(lifetime) => {
                    self.lifetime_mentions_generic(lifetime)
                }
                rustdoc_types::PreciseCapturingArg::Param(param) => {
                    self.names.types.contains_key(param) || self.names.consts.contains_key(param)
                }
            }),
        }
    }

    fn param_mentions_existing_generic(&self, param: &GenericParamDef) -> bool {
        // Stable Rust currently only exposes lifetimes in the HRTB-style binders
        // that item-owner signatures use, but `rustdoc_types` permits richer
        // generic params here. Keep this traversal complete so a future rustdoc
        // shape cannot silently sort an owner-dependent subject with the
        // concrete-subject predicates.
        match &param.kind {
            GenericParamDefKind::Lifetime { outlives } => outlives
                .iter()
                .any(|lifetime| self.lifetime_mentions_generic(lifetime)),
            GenericParamDefKind::Type {
                bounds, default, ..
            } => {
                bounds
                    .iter()
                    .any(|bound| self.generic_bound_mentions_generic(bound))
                    || default
                        .as_ref()
                        .is_some_and(|type_| self.type_mentions_generic(type_))
            }
            GenericParamDefKind::Const { type_, default } => {
                self.type_mentions_generic(type_)
                    || default
                        .as_ref()
                        .is_some_and(|expr| self.const_expr_mentions_generic(expr))
            }
        }
    }

    fn term_mentions_generic(&self, term: &Term) -> bool {
        match term {
            Term::Type(type_) => self.type_mentions_generic(type_),
            Term::Constant(constant) => self.constant_mentions_generic(constant),
        }
    }

    fn constant_mentions_generic(&self, constant: &rustdoc_types::Constant) -> bool {
        self.const_expr_mentions_generic(
            constant.value.as_deref().unwrap_or(constant.expr.as_str()),
        )
    }

    fn const_expr_mentions_generic(&self, expr: &str) -> bool {
        self.names
            .consts
            .keys()
            .any(|name| expr_contains_identifier(expr, name))
    }

    fn lifetime_mentions_generic(&self, lifetime: &str) -> bool {
        self.names.lifetimes.contains_key(lifetime)
    }
}

/// Result of formatting a trait path that may contain associated-bound constraints.
struct TraitPathFormat<'a> {
    /// Trait path text kept in the original bound predicate.
    ///
    /// For `Trait<A, Assoc = B, Other: Bound>`, this is
    /// `Trait<A, Assoc = B>`: equality constraints stay attached to the trait
    /// path, while bound constraints are lifted out.
    bound_text: String,
    /// Trait path text suitable for `<Subject as Trait>::Assoc` projections.
    ///
    /// For `Trait<A, Assoc = B, Other: Bound>`, this is `Trait<A>`, producing a
    /// lifted subject like `<Subject as Trait<A>>::Other`.
    projection_text: String,
    /// Associated-bound constraints that should be lifted into separate predicates.
    constraints: Vec<&'a rustdoc_types::AssocItemConstraint>,
}

/// Prefix predicates lifted from nested associated bounds with their outer binder.
///
/// The enclosing `where`-predicate binder is handled by `PredicateAccumulator`.
/// This helper is only for recursive associated-bound lifting, where a nested
/// projection inherits the binder attached to its parent projection.
fn prefix_lifted_binders(outer_binder: &Binder, predicates: &mut [LiftedBoundPredicate]) {
    if outer_binder.is_empty() {
        return;
    }

    for predicate in predicates {
        predicate.binder = outer_binder.append(&predicate.binder);
    }
}

fn absolute_path(components: &[impl AsRef<str>]) -> String {
    let capacity = components
        .iter()
        .map(|component| "::".len() + component.as_ref().len())
        .sum();
    let mut output = String::with_capacity(capacity);
    for component in components {
        output.push_str("::");
        output.push_str(component.as_ref());
    }
    output
}

fn needs_parens_before_bounds(type_: &Type) -> bool {
    match type_ {
        Type::DynTrait(dyn_trait) => {
            dyn_trait.traits.len() + usize::from(dyn_trait.lifetime.is_some()) > 1
        }
        Type::ImplTrait(bounds) => bounds.len() > 1,
        _ => false,
    }
}

fn format_function_header(header: &rustdoc_types::FunctionHeader) -> String {
    let mut output = String::new();
    if header.is_const {
        output.push_str("const ");
    }
    if header.is_async {
        output.push_str("async ");
    }
    if header.is_unsafe {
        output.push_str("unsafe ");
    }

    match &header.abi {
        rustdoc_types::Abi::Rust => {}
        rustdoc_types::Abi::C { unwind } => push_abi(&mut output, "C", *unwind),
        rustdoc_types::Abi::Cdecl { unwind } => push_abi(&mut output, "cdecl", *unwind),
        rustdoc_types::Abi::Stdcall { unwind } => push_abi(&mut output, "stdcall", *unwind),
        rustdoc_types::Abi::Fastcall { unwind } => push_abi(&mut output, "fastcall", *unwind),
        rustdoc_types::Abi::Aapcs { unwind } => push_abi(&mut output, "aapcs", *unwind),
        rustdoc_types::Abi::Win64 { unwind } => push_abi(&mut output, "win64", *unwind),
        rustdoc_types::Abi::SysV64 { unwind } => push_abi(&mut output, "sysv64", *unwind),
        rustdoc_types::Abi::System { unwind } => push_abi(&mut output, "system", *unwind),
        rustdoc_types::Abi::Other(other) => output.push_str(&format!(r#"extern "{other}" "#)),
    }

    output
}

fn push_abi(output: &mut String, name: &str, unwind: bool) {
    output.push_str(r#"extern ""#);
    output.push_str(name);
    if unwind {
        output.push_str("-unwind");
    }
    output.push_str(r#"" "#);
}

fn assert_not_synthetic_owner_param(param: &GenericParamDef) {
    if let GenericParamDefKind::Type {
        is_synthetic: true, ..
    } = param.kind
    {
        unreachable!(
            "synthetic generic parameter `{}` appeared in an item-owner normalized generic \
             signature; synthetic `impl Trait` parameters are not valid in struct, enum, union, \
             or trait signatures",
            param.name
        )
    }
}

fn letter_name(mut index: usize) -> String {
    let mut output = String::new();

    loop {
        let offset = (index % 26) as u8;
        output.insert(0, char::from(b'a' + offset));
        if index < 26 {
            break;
        }
        index = (index / 26) - 1;
    }

    output
}

fn expr_contains_identifier(expr: &str, identifier: &str) -> bool {
    expr.split(|character: char| character != '_' && !character.is_ascii_alphanumeric())
        .any(|token| token == identifier)
}

#[cfg(test)]
mod tests {
    use rustdoc_types::{GenericParamDef, GenericParamDefKind, Type};

    use super::{Binder, NameMap};

    #[test]
    fn binder_append_handles_top_level_empty_binders() {
        // Ordinary top-level `where` predicates have no higher-ranked binder,
        // so lifted associated-bound predicates often append to an empty outer
        // binder.
        let empty = Binder::from_params([]);
        let outer = Binder::from_params(vec!["'a".to_string()]);
        let inner = Binder::from_params(vec!["'b".to_string()]);

        assert_eq!(
            Binder::from_params([]),
            empty.append(&Binder::from_params([]))
        );
        assert_eq!(outer, outer.append(&Binder::from_params([])));
        assert_eq!(inner, Binder::from_params([]).append(&inner));
    }

    #[test]
    fn binder_append_keeps_outer_params_first() {
        let outer = Binder::from_params(vec!["'a".to_string(), "T1".to_string()]);
        let inner = Binder::from_params(vec!["'b".to_string(), "T2".to_string()]);

        assert_eq!(
            Binder::from_params(vec![
                "'a".to_string(),
                "T1".to_string(),
                "'b".to_string(),
                "T2".to_string(),
            ]),
            outer.append(&inner),
        );
    }

    #[test]
    fn name_map_lifetime_normalizes_rustdoc_lifetimes() {
        let params = [lifetime_param("'source")];
        let names = NameMap::new(&params);

        assert_eq!("'a", names.lifetime("'source"));
        assert_eq!("'static", names.lifetime("'static"));
        assert_eq!("'_", names.lifetime("'_"));
    }

    #[test]
    fn name_map_type_and_const_names_use_separate_sequences() {
        let params = [type_param("Source"), const_param("COUNT")];
        let names = NameMap::new(&params);

        assert_eq!("T1", names.type_name("Source"));
        assert_eq!("C1", names.const_name("COUNT"));
        assert_eq!("T1", names.type_or_const_name("Source"));
        assert_eq!("C1", names.type_or_const_name("COUNT"));
    }

    fn lifetime_param(name: &str) -> GenericParamDef {
        GenericParamDef {
            name: name.to_string(),
            kind: GenericParamDefKind::Lifetime {
                outlives: Vec::new(),
            },
        }
    }

    fn type_param(name: &str) -> GenericParamDef {
        GenericParamDef {
            name: name.to_string(),
            kind: GenericParamDefKind::Type {
                bounds: Vec::new(),
                default: None,
                is_synthetic: false,
            },
        }
    }

    fn const_param(name: &str) -> GenericParamDef {
        GenericParamDef {
            name: name.to_string(),
            kind: GenericParamDefKind::Const {
                type_: Type::Primitive("usize".to_string()),
                default: None,
            },
        }
    }
}
