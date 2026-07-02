pub struct StringHolder {
    content: String,
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
