// There's no shadowing across glob re-exports of the same name
// if it's the same item both times.
//
// This crate exports the name `Foo`.

mod a {
    pub struct Foo;
}
mod b {
    pub use super::a::Foo;
}

pub use a::*;
pub use b::*;
