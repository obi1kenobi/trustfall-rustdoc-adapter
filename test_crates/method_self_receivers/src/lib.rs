#![feature(arbitrary_self_types)]

pub struct Example(i64);

pub struct CustomReceiver<T>(T);

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

impl Example {
    pub fn by_ref(&self) {}

    pub fn by_mut_ref(&mut self) {}

    pub fn by_value(self) {}

    // From the caller's perspective, the receiver here is still `Self`.
    // The `mut` communicates information about the function body
    // which is invisible and irrelevant to the caller.
    pub fn by_mut_value(mut self) {
        self.0 += 1;
    }

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

    pub fn by_custom_receiver_value(self: CustomReceiver<Self>) {}

    pub fn by_custom_receiver_ref(self: &CustomReceiver<Self>) {}

    pub fn by_custom_receiver_mut_ref(self: &mut CustomReceiver<Self>) {}

    pub fn by_custom_receiver_with_ref_self(self: CustomReceiver<&'_ Self>) {}

    pub fn by_pinned_box(self: std::pin::Pin<Box<Self>>) {}

    pub fn by_pinned_ref_arc(self: std::pin::Pin<&std::sync::Arc<Self>>) {}

    pub fn wrong_self(selfless: ()) {}
}
