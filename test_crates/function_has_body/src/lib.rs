pub fn top_level() {}

pub struct Foo;

impl Foo {
    pub fn inside_impl_block() {}
}

pub trait Bar {
    fn trait_no_body();

    fn trait_with_body();
}

extern "C" {
    pub fn extern_no_body();
}
