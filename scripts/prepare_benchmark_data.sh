#!/bin/sh

aws_sdk_version="${AWS_SDK_VERSION:-release-2024-08-22}"

bail() {
	echo "ERROR: $1"
	exit 1
}

repo_path="$(realpath "$(dirname "$0")")"/..
bench_data_path="${repo_path}/localdata/benches/aws-sdk-ec2.json"

mkdir -p "${repo_path}/localdata/benches"

if [ -f "$bench_data_path" ]; then
	echo "Reusing bench input data: already present at $bench_data_path"
	echo "help: delete the file if you want to regenerate it"
	echo "      rm -v '$bench_data_path'"
	exit 0
fi

# FIXME: This is a Linux specific path, on MacOS it should instead point to $HOME/Library/Caches/
#        see: https://docs.rs/directories/latest/directories/struct.ProjectDirs.html#method.cache_dir
cache_path="${XDG_CACHE_HOME:-$HOME/.cache}"
download_url="https://github.com/awslabs/aws-sdk-rust/archive/refs/tags/${aws_sdk_version}.tar.gz"

source_tarball="${cache_path}/aws-sdk-rust-${aws_sdk_version}.tar.gz"

if [ -f "$source_tarball" ]; then
	echo "Reusing source tarball: already present at $source_tarball"
	echo "help: delete the file if you want to download it again"
	echo "      rm -v '$source_tarball'"
else
	echo "Downloading sources..."
	curl --proto '=https' --tlsv1.2 -SfL "$download_url" -o "$source_tarball" || bail "failed to download aws-sdk-rust sources"
fi

source_dir="${cache_path}/aws-sdk-rust-${aws_sdk_version}"

if [ -d "$source_dir" ]; then
	echo "Reusing extracted sources: already present at $source_dir"
	echo "help: delete the directory if you want to extract the source tarball again"
	echo "      rm -rv '$source_dir'"
else
	echo "Extracting sources..."
	tar -x -f "${source_tarball}" -C "${cache_path}"
fi

cd "${source_dir}/sdk/ec2" || bail "failed to cd into the extracted sources"

echo "Generating rustdoc JSON..."
RUSTDOCFLAGS="-Z unstable-options --document-private-items --document-hidden-items --output-format=json --cap-lints=allow" cargo +nightly doc --lib --no-deps || bail "failed to generate rustdoc JSON"

cp -v "$source_dir/target/doc/aws_sdk_ec2.json" "$bench_data_path"
