extern "C" {
    pub fn legacy_extern_fn();
}

unsafe extern "C" {
    pub fn implicit_unsafe_extern_fn();

    pub unsafe fn explicit_unsafe_extern_fn();

    pub safe fn safe_extern_fn();
}
