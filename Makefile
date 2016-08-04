.PHONY: all test build doc release
export RUST_BACKTRACE := 1
export RUST_LOG := rusty_scheme::alloc=debug,rusty_scheme::api=debug
export TARGETS := $(TARGETS)
all: doc
build:
	cargo clippy --features=clippy

test: build
	cargo test -- ${TARGETS}

release: test
	cargo build --release
	cargo test --release

doc: release
	cargo doc
