.PHONY: all test build doc release
export RUST_BACKTRACE := 0
export RUST_LOG := rusty_scheme::alloc=debug,rusty_scheme::api=debug,rusty_scheme::read=debug
export TARGETS := $(TARGETS)
all: doc
build:
	cargo build -j10
	cargo clippy --features=clippy -j10

test: build
	cargo test  -j10 -- ${TARGETS}

release: test
	cargo build --release -j10
	cargo test --release -j10

doc: release
	cargo doc -j10
