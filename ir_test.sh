#!/bin/bash

test_cases=(
    fibonacci.c
    fib2.c
    fib3.c
    fib4.c
    fib5.c
    bar.c
    alignof.c
    array.c
    array2.c
    array3.c
    simple_cond.c
    simple_for.c
    simple_if.c
    simple.c
    sizeof.c
    pointer.c
    return_void.c
    logical_op.c
    gcd.c
    cmp.c
    array4.c
    array5.c
    bitwise.c
    comma.c
    complete_cond.c
    cond_and_loop.c
    cond.c
    float.c
    float2_gen.py
    float2.c
    foo.c
    foo2.c
    foo3.c
    foo4.c
    for_continue_break.c
    integer_literal.c
    integer_literal2.c
    minus_constant.c
    negate.c
    shift.c
    struct.c
    struct2.c
    struct3.c
    switch.c
    temp.c
    temp2.c
    test.c
    typecast.c
    typedef.c
    unary.c
    while_continue_break.c
)

for test_case in "${test_cases[@]}"; do
    cargo run --release --bin fuzz "examples/c/${test_case}" -i
    if [ $? -ne 0 ]; then
        cargo run -- -i "examples/c/${test_case}" | tee temp.ir
        diff temp.ir "examples/ir0/${test_case%c}ir"
        exit 1
    fi
done
