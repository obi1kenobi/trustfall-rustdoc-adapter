use std::sync::OnceLock;

use criterion::{criterion_group, criterion_main, Criterion};
use rustdoc_types::Crate;
use trustfall_rustdoc_adapter::IndexedCrate;

/// Benchmark [`IndexedCrate::new`] with the aws-sdk-ec2 crate as an input
fn new(c: &mut Criterion) {
    // Force the evaluation of the `LazyLock`,
    // because its loading time isn't what we're benchmarking.
    let _ = std::sync::LazyLock::force(&CURRENT_TARGET_TRIPLE);

    let mut group = c.benchmark_group("IndexedCrate");
    let crate_ = get_aws_sdk_crate();
    group.bench_function("new(aws-sdk-ec2)", |b| {
        b.iter_with_large_drop(|| IndexedCrate::new(crate_, &CURRENT_TARGET_TRIPLE))
    });
    group.finish();
}

static AWS_SDK_EC2_CRATE: OnceLock<Crate> = OnceLock::new();

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

fn get_aws_sdk_crate() -> &'static Crate {
    AWS_SDK_EC2_CRATE.get_or_init(|| {
        let data = std::fs::read_to_string("localdata/benches/aws-sdk-ec2.json")
            .expect("failed to read the rustdoc JSON. Did you forget to run `scripts/prepare_benchmark_data.sh`?");
        serde_json::from_str(data.as_str()).expect("benches/aws-sdk-ec2.json appears to contain invalid JSON")
    })
}

criterion_group!(benches, new);

criterion_main!(benches);
