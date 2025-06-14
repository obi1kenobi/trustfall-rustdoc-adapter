use std::fs::read_to_string;

use anyhow::Context;
use rustdoc_types::Crate;

#[derive(serde::Deserialize)]
struct RustdocFormatVersion {
    format_version: u32,
}

pub(crate) fn detect_rustdoc_format_version(file_data: &str) -> anyhow::Result<u32> {
    let version = serde_json::from_str::<RustdocFormatVersion>(file_data)
        .with_context(|| "file does not appear to be a rustdoc JSON format".to_string())?;

    Ok(version.format_version)
}

pub(crate) fn load_pregenerated_rustdoc(crate_name: &str) -> Crate {
    let path = format!("./localdata/test_data/{crate_name}/rustdoc.json");
    let content = read_to_string(&path)
        .with_context(|| format!("Could not load {path} file, did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"))
        .expect("failed to load rustdoc");
    serde_json::from_str(&content)
        .with_context(|| {
            let format_version = detect_rustdoc_format_version(&content);
            match format_version {
                Ok(format_version) => {
                    format!(
                        "Failed to parse {path} file: it is rustdoc version {format_version} but expected {}. \
                        Did you forget to run ./scripts/regenerate_test_rustdocs.sh ?",
                        rustdoc_types::FORMAT_VERSION,
                    )
                }
                Err(..) => {
                    format!(
                        "Failed to parse {path} file: it didn't seem to be valid rustdoc JSON. \
                        Did you forget to run ./scripts/regenerate_test_rustdocs.sh ?"
                    )
                }
            }
        }).expect("failed to parse rustdoc JSON")
}

pub(crate) static CURRENT_TARGET_TRIPLE: std::sync::LazyLock<&str> =
    std::sync::LazyLock::new(|| {
        let outcome = std::process::Command::new("rustc")
            .arg("-vV")
            .output()
            .expect("failed to run `rustc -vV`");
        let stdout = String::from_utf8(outcome.stdout).expect("stdout was not valid utf-8");
        let target_triple = stdout
            .lines()
            .find_map(|line| line.strip_prefix("host: "))
            .expect("failed to find host line");
        target_triple.to_string().leak()
    });
