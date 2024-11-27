use crate::attributes::Attribute;

/// Get the externally-visible name of the specified function item, if any.
///
/// Most Rust functions do not have an externally-visible name. Only functions intended
/// to be called from "outside" the program via FFI have external names, which are set
/// in one of the following ways:
/// ```rust
/// #[no_mangle]  // visible as `externally_visible`
/// fn externally_visible() {}
///
/// #[export_name = "also_externally_visible"]
/// fn internal_name() {}
/// ```
///
/// For all other functions, this function returns `None`.
/// If this function is called with a non-function item, the result is unspecified.
pub(crate) fn function_export_name(item: &rustdoc_types::Item) -> Option<&str> {
    if item.attrs.iter().any(|attr| attr == "#[no_mangle]") {
        // Items with `#[no_mangle]` attributes are exported under their item name.
        // Ref: https://doc.rust-lang.org/reference/abi.html#the-no_mangle-attribute
        item.name.as_deref()
    } else {
        item.attrs
            .iter()
            .filter_map(|attr| {
                if attr.starts_with("#[export_name") {
                    let parsed = Attribute::new(attr);
                    if parsed.content.base == "export_name" {
                        parsed
                            .content
                            .assigned_item
                            .map(|name| name.trim_matches('"'))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .next()
    }
}
