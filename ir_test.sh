#!/bin/bash

test_cases=(
    alignof.c
    array.c
    array2.c
    array3.c
    gcd.c
    pointer.c
    return_void.c
    simple_for.c
    simple_if.c
    simple_cond.c
)

for test_case in "${test_cases[@]}"; do
    cargo run --release --bin fuzz "examples/c/${test_case}" -i
    if [ $? -ne 0 ]; then
        cargo run -- -i "examples/c/${test_case}"
    fi
done
