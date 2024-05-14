use dummy_ext::DummyExternalTrait;
pub trait Trait {}

pub struct StringHolder {
    content: String,
}

impl Trait for StringHolder {}
impl DummyExternalTrait for StringHolder {
    fn do_something(&self) {}
}

impl<'a> PartialEq<StringHolder> for &'a str {
    fn eq(&self, other: &StringHolder) -> bool {
        (*self).eq(&other.content)
    }
}

impl<'a> PartialEq<str> for StringHolder {
    fn eq(&self, other: &str) -> bool {
        self.content.eq(other)
    }
}

impl<'a> PartialEq<&'a str> for StringHolder {
    fn eq(&self, other: &&'a str) -> bool {
        self.content.eq(*other)
    }
}
