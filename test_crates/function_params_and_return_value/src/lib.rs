pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn fn_returns_nothing() {}

pub struct Example;

impl Example {
    pub fn add_method(&self, left: u64, right: u64) -> u64 {
        left + right
    }

    pub fn method_returns_nothing(&self) {}
}

pub trait MyTrait {
    fn add_trait_fn(&self, value: u64) -> u64;

    fn trait_fn_returns_nothing(&self);
}
