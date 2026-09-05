# Fuzzing

This crate contains the fuzz targets for the project.
Currently, only the parser is fuzzed (see [`fuzz_targets/parser.rs`](fuzz_targets/parser.rs))

## Requirements

Fuzzing requires a nightly toolchain (pinned by [`rust-toolchain.toml`](rust-toolchain.toml) in this
directory) and `cargo fuzz` (install via `cargo install cargo-fuzz`).

## Running

Run the following from this directory:

```shell
mkdir -p corpus/parser
cargo fuzz run parser corpus/parser seeds/parser -- -dict=vhdl.dict
```

[`seeds/parser`](seeds/parser) is tracked and contains sensible starting points.

Found crashes are minimized into `artifacts/parser`. Each should be turned into a unit test in the failing crate.

## CI

Every push and pull request builds the fuzz crate and checks that it is rustfmt-clean and free of clippy warnings.

A separate weekly job ([`fuzz.yml`](../.github/workflows/fuzz.yml)), runs the fuzzer.
The job can also be started manually via `workflow_dispatch`, which takes the fuzzing duration in seconds as an input.
