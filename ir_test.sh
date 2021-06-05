#!/bin/bash
cargo run --release --bin fuzz examples/c/alignof.c -i
cargo run --release --bin fuzz examples/c/array.c -i
cargo run --release --bin fuzz examples/c/array2.c -i
cargo run --release --bin fuzz examples/c/array3.c -i
cargo run --release --bin fuzz examples/c/gcd.c -i
cargo run --release --bin fuzz examples/c/pointer.c -i
cargo run --release --bin fuzz examples/c/simple_for.c -i