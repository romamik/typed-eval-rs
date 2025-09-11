test-nightly:
    cargo +nightly test --features nightly

test-stable:
    cargo test

test-all: test-stable && test-nightly