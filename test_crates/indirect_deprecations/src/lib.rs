mod private {
    pub fn first() {}
}

#[deprecated]
pub mod deprecated {
    pub use super::private::first;

    pub fn second() {}
}

#[deprecated]
mod private_deprecated {
    pub fn third() {}
}

pub mod public {
    pub use super::private_deprecated::third;
}
