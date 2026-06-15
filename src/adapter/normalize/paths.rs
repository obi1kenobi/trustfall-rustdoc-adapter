use crate::PackageIndex;

/// Return a normalized representation of the given rustdoc path.
///
/// For items in the current crate, use a publicly importable path when one is
/// available so reexports normalize to the spelling downstream users can name.
/// For other resolved items, fall back to rustdoc's path summary. The raw path
/// fallback is only for rustdoc data that did not resolve to either source.
pub(super) fn normalized_path(crate_: &PackageIndex<'_>, path: &rustdoc_types::Path) -> String {
    if crate_.own_crate.inner.index.contains_key(&path.id) {
        let importable_paths = crate_
            .own_crate
            .visibility_tracker
            .collect_publicly_importable_names(path.id.0);
        // Normalized signatures should use a public API spelling when one exists.
        // The `importable_path` edge has query-observable order, so choose here
        // without reordering that edge.
        let importable_path = importable_paths.iter().min_by_key(|importable_path| {
            (
                !importable_path.public_api(),
                importable_path.modifiers.doc_hidden,
                importable_path.modifiers.deprecated,
            )
        });
        if let Some(importable_path) = importable_path {
            return absolute_path(importable_path.path.components.iter().copied());
        }
    }

    if let Some(summary) = crate_.own_crate.inner.paths.get(&path.id) {
        return absolute_path(summary.path.iter().map(String::as_str));
    }

    assert_ne!(path.path, "");
    if path.path.starts_with("::") {
        path.path.clone()
    } else {
        format!("::{}", path.path)
    }
}

fn absolute_path<'a>(components: impl ExactSizeIterator<Item = &'a str>) -> String {
    let length = components.len();
    let mut output = String::with_capacity(10 * length);
    for component in components {
        output.push_str("::");
        output.push_str(component);
    }
    output
}
