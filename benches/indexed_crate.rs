use std::sync::OnceLock;

use criterion::{criterion_group, criterion_main, Criterion};
use rustdoc_types::Crate;
use trustfall_rustdoc_adapter::IndexedCrate;

/// Benchmark [`IndexedCrate::new`] with the aws-sdk-ec2 crate as an input
fn new(c: &mut Criterion) {
    let mut group = c.benchmark_group("IndexedCrate");
    let crate_ = get_aws_sdk_crate();
    group.bench_function("new(aws-sdk-ec2)", |b| {
        b.iter_with_large_drop(|| IndexedCrate::new(crate_))
    });
    group.finish();
}

static AWS_SDK_EC2_CRATE: OnceLock<Crate> = OnceLock::new();

fn get_aws_sdk_crate() -> &'static Crate {
    AWS_SDK_EC2_CRATE.get_or_init(|| {
        let data = std::fs::read_to_string("localdata/benches/aws-sdk-ec2.json")
            .expect("failed to read the rustdoc JSON. Did you forget to run `scripts/prepare_benchmark_data.sh`?");
        serde_json::from_str(data.as_str()).expect("benches/aws-sdk-ec2.json appears to contain invalid JSON")
    })
}

criterion_group!(benches, new);

criterion_main!(benches);
