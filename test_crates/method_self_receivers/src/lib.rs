#![feature(arbitrary_self_types, arbitrary_self_types_pointers)]

use std::io::{Read, Write};
// Renaming both the inner and outer pointers should preserve the receiver kinds
// `Rc<Box<Self>>` and `Pin<&Arc<Self>>`.
use std::{boxed::Box as Owned, pin::Pin as Pinned, rc::Rc as SharedRc, sync::Arc as SharedArc};

pub struct Example(i64);

pub struct CustomReceiver<T>(T);

pub struct GenericExample<'a, T: ?Sized + 'a>(&'a T);

pub trait Runtime {
    type Handle;
}

pub struct Threaded;

impl Runtime for Threaded {
    type Handle = SharedArc<Example>;
}

pub struct Local;

impl Runtime for Local {
    type Handle = SharedRc<Example>;
}

/// Per the docs: <https://doc.rust-lang.org/std/ops/trait.Receiver.html>
///
/// N.B.: `Pin` currently doesn't appear to be able to be combined with custom receivers.
///
/// ```compile_fail
/// struct Example;
///
/// impl Example {
///     pub fn by_pinned_custom_receiver(
///         self: std::pin::Pin<method_self_receivers::CustomReceiver<Self>>,
///     ) {}
/// }
/// ```
impl<T> std::ops::Receiver for CustomReceiver<T> {
    type Target = T;
}

mod other {
    pub struct CustomReceiver<T>(T);

    impl<T> std::ops::Receiver for CustomReceiver<T> {
        type Target = T;
    }
}

pub use other::CustomReceiver as OtherReceiver;

impl Example {
    pub fn by_ref(&self) {}

    // Spelling out `&Example` gives the same receiver kind as `by_ref`'s `&self`.
    pub fn by_explicit_ref(self: &Example) {}

    pub fn by_mut_ref(&mut self) {}

    pub fn by_ref_to_ref(self: &&Self) {}

    pub fn by_ref_to_mut_ref(self: &&mut Self) {}

    pub fn by_mut_ref_to_ref(self: &mut &Self) {}

    pub fn by_mut_ref_to_mut_ref(self: &mut &mut Self) {}

    pub fn by_ref_to_nested_ref(self: &&&Self) {}

    pub fn by_value(self) {}

    // From the caller's perspective, the receiver here is still `Self`.
    // The `mut` communicates information about the function body
    // which is invisible and irrelevant to the caller.
    pub fn by_mut_value(mut self) {
        self.0 += 1;
    }

    pub fn by_const_pointer(self: *const Self) {}

    pub fn by_mut_pointer(self: *mut Self) {}

    pub fn by_const_pointer_to_ref(self: *const &Self) {}

    pub fn by_mut_pointer_to_mut_ref(self: *mut &mut Self) {}

    pub fn by_const_pointer_to_nested_ref(self: *const &&mut Self) {}

    pub fn by_ref_to_const_pointer(self: &*const Self) {}

    pub fn by_mut_ref_to_mut_pointer(self: &mut *mut Self) {}

    pub fn by_nested_pointer(self: *const *mut Self) {}

    pub fn by_boxed_pointer(self: Box<*const Self>) {}

    pub fn by_pinned_mut_ref(self: std::pin::Pin<&mut Self>) {}

    pub fn by_ref_pinned_mut_ref(self: &std::pin::Pin<&'_ mut Self>) {}

    pub fn by_ref_pinned_mut_ref_lifetime<'a>(self: &std::pin::Pin<&'a mut Self>) {}

    pub fn by_mut_ref_pinned_mut_ref(self: &mut std::pin::Pin<&mut Self>) {}

    pub fn by_mut_ref_pinned_mut_ref_lifetime(self: &mut std::pin::Pin<&'_ mut Self>) {}

    pub fn by_boxed_value(self: Box<Self>) {}

    pub fn by_ref_boxed_value(self: &Box<Self>) {}

    pub fn by_mut_ref_boxed_value(self: &mut Box<Self>) {}

    pub fn by_rc_value(self: std::rc::Rc<Self>) {}

    pub fn by_ref_rc_value(self: &std::rc::Rc<Self>) {}

    pub fn by_mut_ref_rc_value(self: &mut std::rc::Rc<Self>) {}

    pub fn by_arc_value(self: std::sync::Arc<Self>) {}

    pub fn by_ref_arc_value(self: &std::sync::Arc<Self>) {}

    pub fn by_mut_ref_arc_value(self: &mut std::sync::Arc<Self>) {}

    pub fn by_box_of_rc_ref(self: &std::rc::Rc<Box<Self>>) {}

    pub fn by_renamed_rc_box(self: &SharedRc<Owned<Self>>) {}

    pub fn by_custom_receiver_value(self: CustomReceiver<Self>) {}

    // Both methods report `CustomReceiver<Self>` because their receiver structs share a name.
    // If `by_custom_receiver_value` took `OtherReceiver<Self>` like this method,
    // callers could no longer pass `CustomReceiver<Example>`.
    pub fn by_other_receiver_value(self: OtherReceiver<Self>) {}

    pub fn by_custom_receiver_ref(self: &CustomReceiver<Self>) {}

    pub fn by_custom_receiver_mut_ref(self: &mut CustomReceiver<Self>) {}

    pub fn by_custom_receiver_with_ref_self(self: CustomReceiver<&'_ Self>) {}

    // Both bounds belong to the pointee, so the tag must retain the parentheses.
    pub fn by_dyn_read_send_pointer_tag(self: Tagged<Self, *const (dyn Read + Send)>) {}

    // The explicit object lifetime counts as a second bound, so the pointee needs
    // parentheses even though `kind` omits the lifetime on the reference.
    pub fn by_dyn_read_lifetime_ref_tag<'a>(self: Tagged<Self, &'a (dyn Read + 'a)>) {}

    pub fn by_pinned_box(self: std::pin::Pin<Box<Self>>) {}

    pub fn by_pinned_ref_arc(self: std::pin::Pin<&std::sync::Arc<Self>>) {}

    pub fn by_renamed_pinned_arc(self: Pinned<&SharedArc<Self>>) {}

    // Rust accepts the associated type as a receiver because it equals `Arc<Example>`.
    // The receiver kind preserves the spelling `<Threaded as Runtime>::Handle`.
    pub fn by_runtime_handle(self: <Threaded as Runtime>::Handle) {}

    pub fn wrong_self(selfless: ()) {}
}

impl<'a, T: ?Sized + 'a> GenericExample<'a, T> {
    pub fn by_generic_value(self: GenericExample<'a, T>) -> &'a T {
        self.0
    }

    pub fn by_generic_ref(self: &GenericExample<'a, T>) -> &&'a T {
        &self.0
    }
}

pub struct Reader<T = Vec<u8>, const CAPACITY: usize = 4>(T);
pub use Reader as ReexportedReader;

impl<Element> Reader<Element> {
    // `ReexportedReader` and `Reader` name the same declaration.
    // Both take the impl parameter `Element`, so the receiver has kind `Box<Self>`.
    pub fn by_boxed_reexport(self: Box<crate::ReexportedReader<Element>>) {}
}

impl Reader<u8> {
    // The concrete argument `u8` is part of the match that gives this receiver kind `Self`.
    pub fn by_concrete_ref(self: &Reader<u8>) {}

    // The `Receiver` impl below lets method lookup reach `Reader<u8>` from `Reader<u16>`.
    // The caller still supplies `CustomReceiver<Reader<u16>>`, so replacing the inner
    // type with `Self` would report the wrong parameter type.
    pub fn by_other_instantiation(self: CustomReceiver<Reader<u16>>) {}
}

impl std::ops::Receiver for Reader<u16> {
    type Target = Reader<u8>;
}

impl<'a, const WIDTH: usize> Reader<&'a [[u8; WIDTH]]> {
    // The reference and the array length use the impl's own `'a` and `WIDTH`,
    // so the entire `Reader` inside `Box` should render as `Self`.
    pub fn by_rows(self: Box<Reader<&'a [[u8; WIDTH]]>>) {}
}

impl Reader<fn(*const u8) -> (u8, usize)> {
    // Naming the callback parameter `bytes` leaves its function pointer type unchanged,
    // so the explicit `Reader` still denotes `Self`.
    pub fn by_callback(self: Box<Reader<fn(bytes: *const u8) -> (u8, usize)>>) {}
}

impl Reader<Box<dyn Read>> {
    // Both trait objects omit the lifetime bound in `Box<dyn Read>`,
    // so they acquire the same default and the receiver has kind `Box<Self>`.
    pub fn by_boxed_dyn_read(self: Box<Reader<Box<dyn Read>>>) {}
}

impl Reader<Box<dyn Read + 'static>> {
    // Both trait objects have the same explicit lifetime. Only the `Read` argument
    // matches the impl, so the `Reader` with the `Write` argument stays in the tag.
    pub fn by_dyn_write_tag(
        self: Tagged<Reader<Box<dyn Read + 'static>>, Reader<Box<dyn Write + 'static>>>,
    ) {
    }
}

// The same associated type appears in the impl and in `by_projected_handle`,
// so the boxed receiver has kind `Box<Self>`. Keep the explicit `Reader` in
// `by_local_tag` because `Local` selects an `Rc` handle and `Threaded` selects an `Arc`.
impl Reader<<Threaded as Runtime>::Handle> {
    pub fn by_projected_handle(self: Box<Reader<<Threaded as Runtime>::Handle>>) {}

    pub fn by_local_tag(self: Tagged<Self, Reader<<Local as Runtime>::Handle>>) {}
}

impl<const N: usize> Reader<u8, N> {
    // Render the target as `Self` because it repeats the impl's `N`. Keep `Reader<u8, 8>`
    // in the tag because the receiver kind must also describe calls where `N` differs from `8`.
    pub fn by_fixed_capacity_tag(self: Tagged<Reader<u8, N>, Reader<u8, 8>>) {}
}

// Either the impl or the receiver may omit trailing defaults. Here the receiver
// spells out both defaults. In `by_omitted_const` it omits just the capacity.
// Both receiver types are `Box<Self>`.
impl Reader {
    pub fn by_explicit_defaults(self: Box<Reader<Vec<u8>, 4>>) {}
}

impl Reader<Vec<u8>, 4> {
    pub fn by_omitted_const(self: Box<Reader<Vec<u8>>>) {}
}

// The receiver targets `T`. Its tag lets a test include a second type in the
// receiver and check independently whether that type also denotes `Self`.
pub struct Tagged<T, Tag = T>(T, std::marker::PhantomData<Tag>);

impl<T, Tag> std::ops::Receiver for Tagged<T, Tag> {
    type Target = T;
}

// The default tag repeats the first argument, so all three methods take `Box<Self>`.
impl<Element> Tagged<Element> {
    pub fn by_boxed_self(self: Box<Self>) {}

    pub fn by_omitted_tag(self: Box<Tagged<Element>>) {}

    pub fn by_explicit_tag(self: Box<Tagged<Element, Element>>) {}
}

impl<T> Tagged<Vec<T>> {
    // The default tag takes the first argument of `Tagged`, which is `Vec<T>` here.
    // The inner `Tagged<Vec<T>, T>` uses the impl's `T` as its tag, so it differs from `Self`.
    pub fn by_other_tag(self: Tagged<Self, Tagged<Vec<T>, T>>) {}
}

pub struct Chain<A, B = A, C = B>(A, B, C);

impl<T> Chain<T> {
    // The third argument copies the second, which the impl also omits.
    pub fn by_chained_defaults(self: Box<Chain<T, T, T>>) {}
}

pub struct Batch<A, Row = Vec<A>, Rows = Vec<Vec<A>>>(A, Row, Rows);

impl<A> Batch<Option<A>> {
    // The declaration's `A` takes the value `Option<A>` from this impl.
    // Each explicit default below must use that entire argument.
    pub fn by_explicit_row(self: Box<Batch<Option<A>, Vec<Option<A>>>>) {}

    pub fn by_explicit_rows(self: Box<Batch<Option<A>, Vec<Option<A>>, Vec<Vec<Option<A>>>>>) {}

    // The tag uses the impl's `A` directly, so its row differs from the default.
    pub fn by_other_element(self: Tagged<Self, Batch<Option<A>, Vec<A>>>) {}

    // The element matches, but the tag stores it in an `Option` instead of a `Vec`.
    pub fn by_other_container(self: Tagged<Self, Batch<Option<A>, Option<Option<A>>>>) {}
}

pub struct Stream<R = Reader>(R);

impl Stream {
    // `Reader` defaults to `Reader<Vec<u8>, 4>`. The target spells out those defaults,
    // but the tag changes the capacity, so only the target denotes `Self`.
    pub fn by_capacity(self: Tagged<Stream<Reader<Vec<u8>, 4>>, Stream<Reader<Vec<u8>, 8>>>) {}
}

pub struct BorrowedPair<'a, First: 'a, Second = First>(&'a First, Second);

impl<'a> BorrowedPair<'a, *mut dyn Read> {
    // The first pointer's `dyn Read` defaults to `'a` because `First: 'a`.
    // Omitting `Second` copies that pointer type. Spelling it out in the tag lets
    // the second pointer's object lifetime default to `'static`, so the tag differs from `Self`.
    pub fn by_default_object_lifetimes(
        self: Tagged<Self, BorrowedPair<'a, *mut dyn Read, *mut dyn Read>>,
    ) {
    }
}

impl<'a> BorrowedPair<'a, *mut (dyn Read + 'a)> {
    // Both pointers name `'a` explicitly, so the boxed type denotes `Self`.
    pub fn by_named_object_lifetimes(
        self: Box<BorrowedPair<'a, *mut (dyn Read + 'a), *mut (dyn Read + 'a)>>,
    ) {
    }
}

// `Cell` makes `View` invariant. A caller with different lifetimes for the owner
// and the tag can call `by_inferred_lifetime`, but cannot shorten both lifetimes
// to a common one to call `by_named_lifetime`. Only the named lifetime
// establishes that the tag is also `Self`.
pub struct View<'a>(pub std::cell::Cell<&'a u8>);

impl<'a> View<'a> {
    pub fn by_named_lifetime(self: Tagged<Self, View<'a>>) {}
}

impl View<'_> {
    pub fn by_inferred_lifetime(self: Tagged<Self, View<'_>>) {}
}

// The default method fixes the tag as `Example` for every implementor.
// Looking up the inherited method through `Example` should preserve that tag.
pub trait TaggedRead: Sized {
    fn by_tag(self: Tagged<Self, Example>) {}
}

impl TaggedRead for Example {}

pub trait BlanketRead: Sized {
    fn by_blanket(self: Tagged<Self, Example>);
}

// Rustdoc also lists this blanket impl on `Example`. The method is declared
// for every `T`, so the tag should still name `Example` when queried through that impl.
impl<T> BlanketRead for T {
    fn by_blanket(self: Tagged<Self, Example>) {}
}

pub enum Frame<const BYTES: usize> {
    Data([u8; BYTES]),
}

impl<const BYTES: usize> Frame<BYTES> {
    // Repeating the impl's `BYTES` parameter makes the tag `Self` for every frame size.
    pub fn by_same_size(self: Tagged<Self, Frame<BYTES>>) {}
}

const HEADER_BYTES: usize = 4;
const PAYLOAD_BYTES: usize = 128;
const CHECKSUM_BYTES: usize = 4;

impl Frame<{ HEADER_BYTES + PAYLOAD_BYTES }> {
    // The checksum makes the tagged frame larger. Rustdoc can render both size
    // expressions as `{ _ }`, so matching their text would wrongly make the tag `Self`.
    pub fn by_checksummed_tag(
        self: Tagged<Self, Frame<{ HEADER_BYTES + PAYLOAD_BYTES + CHECKSUM_BYTES }>>,
    ) {
    }
}
