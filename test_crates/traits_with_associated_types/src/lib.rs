#![feature(associated_type_defaults)]

pub trait Serializer {
    type SerializedType = [u8; 20];
    type DeserializedType;
}
