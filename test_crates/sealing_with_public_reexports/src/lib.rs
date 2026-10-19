#[doc(hidden)]
pub mod hidden {
    pub trait PublicTrait {}
    pub trait HiddenTrait {}

    pub struct PublicToken;
    pub struct HiddenToken;
}

pub use hidden::{PublicToken, PublicTrait};

pub trait PublicMethodInput {
    fn required(value: PublicToken);
}

pub trait HiddenMethodInput {
    fn required(value: hidden::HiddenToken);
}

pub trait PublicMethodReturn {
    fn required() -> PublicToken;
}

pub trait HiddenMethodReturn {
    fn required() -> hidden::HiddenToken;
}

pub trait PublicAssociatedConst {
    const VALUE: PublicToken;
}

pub trait HiddenAssociatedConst {
    const VALUE: hidden::HiddenToken;
}
