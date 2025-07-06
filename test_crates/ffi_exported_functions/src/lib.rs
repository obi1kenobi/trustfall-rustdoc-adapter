#[unsafe(no_mangle)]
pub extern "C" fn top_level_no_mangle_fn() {}

#[unsafe(export_name = "exported")]
pub extern "C-unwind" fn top_level_export_name_fn() {}

#[repr(C)]
pub struct Example;

impl Example {
    #[unsafe(no_mangle)]
    pub extern "C" fn associated_fn() {}

    #[unsafe(export_name = "assoc_exported")]
    pub extern "C-unwind" fn assoc_exported_fn() {}

    #[unsafe(no_mangle)]
    pub extern "C" fn method(&self) {}
}
