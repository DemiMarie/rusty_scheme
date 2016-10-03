#!/bin/sh
export RUST_BACKTRACE=1
exec cargo test api::tests::intern_many_symbols
