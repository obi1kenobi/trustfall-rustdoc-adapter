// There's no shadowing across glob re-exports of the same name
// if it's the same item both times.
//
// This crate exports the name `Foo`.

mod defn {
    pub struct Bar;
}
mod a {
    pub use super::defn::Bar as Foo;
}
mod b {
    pub use super::defn::Bar as Foo;
}

pub use a::*;
pub use b::*;
