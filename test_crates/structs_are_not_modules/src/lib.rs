pub struct Foo {
    // This field is publicly visible, but not importable.
    pub field: (),
}

// The items here are publicly visible, but not importable:
//
//    |
// 19 | use Foo::associated_fn;
//    |     ^^^ `Foo` is a struct, not a module
impl Foo {
    pub const THE_ANSWER: i64 = 42;

    pub fn associated_fn(x: i64, y: i64) -> i64 {
        x + y
    }

    pub fn method(&self, x: i64) -> i64 {
        x
    }
}

// This function is importable.
pub fn top_level_function(x: i64) -> i64 {
    x
}
