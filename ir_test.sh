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
    foo.c
    foo2.c
    foo3.c
    unary.c
    cond.c
    test.c
    typecast.c
    temp.c
    negate.c
    bitwise.c
    comma.c
    shift.c
    foo4.c
    complete_cond.c
    cond_and_loop.c
    minus_constant.c
    integer_literal.c
    integer_literal2.c
    for_continue_break.c
    while_continue_break.c
    # complete!!!
    array4.c
    array5.c
    float.c
    float2.c
    struct.c
    struct2.c
    struct3.c
    switch.c
    temp2.c
    typedef.c
)

for test_case in "${test_cases[@]}"; do
    cargo run --release --bin fuzz "examples/c/${test_case}" -i
    if [ $? -ne 0 ]; then
        cargo run -- -i "examples/c/${test_case}" | tee temp.ir
        diff temp.ir "examples/ir0/${test_case%c}ir"
        exit 1
    fi
done
