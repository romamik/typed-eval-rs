fmt:
    cargo +nightly fmt

test-nightly:
    cargo +nightly test --features nightly

test-stable:
    cargo test

test-all: test-stable && test-nightly

check-nightly:
    cargo +nightly check --features nightly

check-stable:
    cargo check

check-all: check-stable && check-nightly

clippy-nightly:
    cargo +nightly clippy --features nightly

clippy-stable:
    cargo clippy

clippy-all: clippy-stable && clippy-nightly