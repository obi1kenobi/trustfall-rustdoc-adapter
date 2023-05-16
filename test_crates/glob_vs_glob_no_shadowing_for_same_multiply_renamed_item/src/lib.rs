// There's no shadowing across glob re-exports of the same name
// if it's the same item both times.
//
// This crate exports the name `Foo`.

mod defn {
    pub struct Bar;
}
mod intermediate {
    pub use super::defn::Bar as Other;
}
mod a {
    pub use super::intermediate::Other as Foo;
}
mod b {
    pub use super::defn::Bar as Foo;
}

pub use a::*;
pub use b::*;
