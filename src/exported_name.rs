/// Get the externally-visible name of the specified item, if any.
///
/// Most Rust items do not have an externally-visible name. Only items intended
/// to be called or accessed from "outside" the program via FFI have external names,
/// which are set in one of the following ways:
///
/// For functions:
/// ```rust
/// #[unsafe(no_mangle)]  // visible as `externally_visible`
/// fn externally_visible() {}
///
/// #[unsafe(export_name = "also_externally_visible")]
/// fn internal_name() {}
/// ```
///
/// For statics:
/// ```rust
/// #[unsafe(no_mangle)]  // visible as `VAR1`
/// static VAR1: i32 = 42;
///
/// #[unsafe(export_name = "EXTERNALLY_VISIBLE")] // visible as `EXTERNALLY_VISIBLE`
/// static VAR2: i32 = 42;
/// ```
///
/// For all other functions/statics, this function returns `None`.
/// If this function is called with an item that doesn't support external names,
/// the result is unspecified.
pub(crate) fn item_export_name(item: &rustdoc_types::Item) -> Option<&str> {
    // First check for export_name attribute, as it takes precedence:
    // https://github.com/rust-lang/rust/issues/47446
    let export_name = item
        .attrs
        .iter()
        .filter_map(|attr| match attr {
            rustdoc_types::Attribute::ExportName(export_name) => Some(export_name.as_str()),
            _ => None,
        })
        .next();

    export_name.or_else(|| {
        // Check for no_mangle attribute
        // Items with `#[no_mangle]` attributes are exported under their item name.
        // Ref: https://doc.rust-lang.org/reference/abi.html#the-no_mangle-attribute
        if item
            .attrs
            .iter()
            .any(|attr| matches!(attr, rustdoc_types::Attribute::NoMangle))
        {
            item.name.as_deref()
        } else {
            None
        }
    })
}
