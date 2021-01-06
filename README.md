# erlang-pbc

![CI](https://github.com/helium/erlang-pbc/workflows/CI/badge.svg)

Building
--------

Fork the repo and simply use `make` to build the library. You will
need `cmake` installed to build the NIFs.

To run the tests run `make test`.

## Cross compilation

Cross-compilation requires the environment variable `ERTS_INCLUDE_DIR`
defined as the target directory containing `erl_nif.h`,
e.g. `ERTS_INCLUDE_DIR=target/usr/lib/erlang/erts-<VERSION>/include`.
