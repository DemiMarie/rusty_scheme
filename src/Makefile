.POSIX:
.NOTPARALLEL:
.PHONY: all test build doc
export RUST_BACKTRACE := 1
all: test doc
build:
	cargo build

test: build
	cargo test

doc: build
	cargo doc
