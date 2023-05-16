pub enum Foo {
    // Enum variants are the exception: they are both visible and importable.
    Variant,
}

// The items here are publicly visible, but not importable:
//
//    |
// 19 | use Foo::associated_fn;
//    |     ^^^^^^^^^^^^^^^^^^ no `associated_fn` in `Foo`
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
