pub enum EnumWithDrop {
    Unit,
}

impl std::ops::Drop for EnumWithDrop {
    fn drop(&mut self) {}
}

pub struct StructWithDrop;

impl core::ops::Drop for StructWithDrop {
    fn drop(&mut self) {}
}

// Needing drop glue for a field does not imply an implementation of `Drop`.
pub enum EnumWithDropField {
    Value(String),
}

pub enum PlainEnum {
    Unit,
}

pub mod shadow {
    pub trait Drop {}
}

pub enum ShadowDropEnum {
    Unit,
}

impl shadow::Drop for ShadowDropEnum {}
