# KECC: KAIST Educational C Compiler

## Install

Install [rustup](https://rustup.rs/).


## Build

```sh
cargo build            # debug build
cargo build --release  # release build
```


## Run

```sh
cargo run -- -h                                     # print options
cargo run -- -p            examples/c/fibonacci.c   # parse
cargo run -- -i            examples/c/fibonacci.c   # irgen
cargo run -- -O --iroutput examples/c/fibonacci.c   # optimize
cargo run --               examples/c/fibonacci.c   # compile

cargo run -- --irrun examples/c/fibonacci.c   # interprets the IR

cargo run --release -- examples/c/fibonacci.c  # compile with release build
```


## Test

```sh
RUST_MIN_STACK=33554432 cargo test             # debug build test
RUST_MIN_STACK=33554432 cargo test --release   # release build test

RUST_MIN_STACK=33554432 cargo test test_examples_write_c       # run write_c test

RUST_MIN_STACK=33554432 cargo test test_examples_irgen_small   # run irgen test using a small subset of examples
RUST_MIN_STACK=33554432 cargo test test_examples_irgen         # run irgen test

RUST_MIN_STACK=33554432 cargo test test_examples_simplify_cfg  # run simplify_cfg test
RUST_MIN_STACK=33554432 cargo test test_examples_mem2reg       # run mem2reg test
RUST_MIN_STACK=33554432 cargo test test_examples_deadcode      # run deadcode test
RUST_MIN_STACK=33554432 cargo test test_examples_gvn           # run gvn test

RUST_MIN_STACK=33554432 cargo test test_examples_asmgen_small  # run asmgen test using a small subset of examples
RUST_MIN_STACK=33554432 cargo test test_examples_asmgen        # run asmgen test

RUST_MIN_STACK=33554432 cargo test test_examples_end_to_end    # run irgen, optimize and asmgen pipeline test
```

`RUST_MIN_STACK=33554432` is necessary for deep call stack for irgen tests.


## Fuzzing

We encourage you to do homework using the test-driven development approach (TDD). You randomly
generate test input, and if it fails, then reduce it as much as possible and manually inspect the
reduced test input. For example, for homework 1, do:

```sh
# randomly generates test inputs and tests them
python3 tests/fuzz.py --print

# reduces the failing test input as much as possible
python3 tests/fuzz.py --print --reduce

# fix your code for the reduced test input
cat tests/test_reduced.c
```

### Install

```sh
# Ubuntu 18.04 or higher
apt install -y make cmake python3
apt install -y csmith libcsmith-dev creduce
```

### Run

The following script generates 10 random test cases and tests your C AST printer:

```sh
python3 tests/fuzz.py --help        # print options
python3 tests/fuzz.py --print -n10  # test C AST printer for 10 times
```

We use `csmith` to randomly generate C source codes. `csmith` will be automatically downloaded and
built by the test script. For more information, we refer to the
[Csmith](https://embed.cs.utah.edu/csmith/) homepage.

### Reduce

When the fuzzer finds a buggy input program for your compiler, it is highly likely that the input
program is too big to manually inspect. We use `creduce` that reduces the buggy input program as
much as possible.

Suppose `tests/test_polished.c` is the buggy input program. Then the following script reduces the
program to `tests/test_reduced.c`:

```sh
python3 tests/fuzz.py --reduce <fuzz-option>
```

`<fuzz-option>` can be `--print` or `--irgen`. It shall be the one used in [Run](#run).

### How it reduces test case?

The script performs unguided test-case reduction using `creduce`: given a buggy program, it randomly
reduces the program; check if the reduced program still fails on the test, and if so, replaces the
given program with the reduced one; repeat until you get a small enough buggy program. For more
information, we refer to the [Creduce](https://embed.cs.utah.edu/creduce/) homepage.

**[NOTICE]** The fuzzer supports Ubuntu 18.04 or 20.04 only. It may work for other platforms, but if it
doesn't, please run the fuzzer in Ubuntu 18.04 or 20.04.


## Running RISC-V Binaries

### Install

```sh
# Ubuntu 20.04 or higher
apt install gcc-10-riscv64-linux-gnu g++-10-riscv64-linux-gnu qemu-user-static
```

### Cross-Compilation and Architecture-Emulation

```sh
# Compile C source code into RISC-V assembly
riscv64-linux-gnu-gcc-10 hello.c -S -o hello.S

# Link to an RISC-V executable
riscv64-linux-gnu-gcc-10 -static hello.S -o hello

# Emulate the executable
qemu-riscv64-static ./hello

# Check the return value
echo $?
```


## Run Benchmark for Performance Competition

```sh
cd bench
make run
```


## Submission

- Submit the corresponding files to [gg.kaist.ac.kr](https://gg.kaist.ac.kr).
- Run `./scripts/make-submissions.sh` to generate `hw3.zip` to `hw6.zip`, which you should submit for homework 3 to 6.

## Debug

- start qemu gdbserver with `qemu-riscv64-static -g 33669 ./a.out`
- connect qemu gdbserver with following commands:
    ```shell
        $ gdb-multiarch ./a.out
            > target remote 127.0.0.1:33669
            > layout regs
            > layout asm
    ```
- double to hex

```python
import binascii
import struct
struct.unpack('d', binascii.unhexlify("3cfd4f970e804f1"))

def double_to_hex(f):
    return hex(struct.unpack('<Q', struct.pack('<d', f))[0])

```