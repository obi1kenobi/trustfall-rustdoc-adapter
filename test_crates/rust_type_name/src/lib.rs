pub trait MyTrait {
    type A<'a, K: 'a>;
}

struct A;
impl MyTrait for A {
    type A<'a, K: 'a> = Option<&'a K>;
}

pub trait MyTrait2<'a, const N: u8, T = ()> {
    type B;
}

mod a {
    pub type B = ();
}

pub struct Struct<'a, T> {
    pub a: String,
    pub b: T,
    pub c: Option<T>,
    pub d: <A as MyTrait>::A<'static, ()>,
    pub e: a::B,
    pub f: unsafe extern "C-unwind" fn() -> T,
    pub g: Box<dyn for<'b> MyTrait2<'b, b'a', B = &'b ()> + Send + 'a>,
}

const unsafe fn x() {}
