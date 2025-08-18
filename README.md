# Update: 2025-08-19

This repository used to hold two projects: `p4cherry` and `p4-spectec`.
`p4cherry` is an OCaml implementation of the P4 language, while `p4-spectec` is a formal, mechanized specification for P4 using the SpecTec framework.
We have now split these projects into their own repositories for better organization and clarity.
Now, `p4-spectec` can be found at: [P4-SpecTec](https://github.com/kaist-plrg/p4-spectec).

# P4Cherry

An OCaml implementation of the P4 language.
This reuses parts of the [Petr4](https://github.com/verified-network-toolchain/petr4) codebase, especially the parser and numerics implementation.

## Building

* Install `opam` version 2.0.5 or higher.
  ```shell
  $ apt-get install opam
  $ opam init
  ```

* Create OCaml switch for version 5.1.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```shell
  $ opam switch create 5.1.0
  $ eval $(opam env)
  $ opam install dune bignum menhir core core_unix bisect_ppx
  ```

* Install the project.
  ```shell
  $ cd p4
  $ dune build p4cherry.opam 
  $ opam install .
  $ cd ..
  ```

### Building the Project

```shell
$ make build
```

This creates an executable `p4cherry` in the project root.

### Additional Notes

You may also need `libgmp-dev` and `pkg-config`, if the error message says so.

## To Run p4cherry

```shell
$ ./p4cherry parse -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry typecheck -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry instantiate -i p4/testdata/arch [FILENAME].p4
$ ./p4cherry run -a v1model -i p4/testdata/arch [FILENAME].p4 [TESTNAME].stf
```

Note that p4cherry currently only supports the V1Model architecture, and partly the eBPF architecture.

### Current Test Status

You can run the tests against the p4c compiler test suite and petr4 custom test suite with:

```shell
$ make test
```

To measure the coverage of the tests, run:

```shell
$ make coverage
```

This will generate `index.html` in `p4/_coverage`.

#### Parser

Parsing, pretty-printing, and roundtripping ([p4c](p4/test/parse_p4c.expected) / [petr4](p4/test/parse_petr4.expected))

#### Type checker

* Positive type checker tests (well-typed programs should be accepted) ([p4c](p4/test/typecheck_pos_p4c.expected) / [petr4](p4/status/petr4/typecheck_pos_petr4.expected))
* Excluded positive type checker tests ([p4c](p4/test/typecheck_pos_p4c_excluded.expected))
* Negative type checker tests (ill-typed programs should be rejected) ([p4c](p4/test/typecheck_neg_p4c.expected) / [petr4](p4/test/typecheck_neg_petr4.expected))
* Excluded negative type checker tests ([p4c](p4/test/typecheck_neg_p4c_excluded.expected))

Analysis of test failures: [p4c-pos](p4/status/p4c/typecheck-pos.analysis.md) / [p4c-neg](p4/status/p4c/typecheck-neg.analysis.md) / [petr4-pos](p4/status/petr4/typecheck-pos.analysis.md)

#### Instantiator

Instantiation of stateful objects ([p4c](p4/test/instantiate_p4c.expected) / [petr4](p4/test/instantiate_petr4.expected))

#### Interpreter

Running STF tests against the p4c compiler test suite and petr4 custom test suite (for V1Model) ([p4c](p4/test/run_v1model_p4c.expected) / [petr4](p4/test/run_v1model_petr4.expected))

Analysis of test failures: [p4c](p4/status/p4c/run-v1model.analysis.md) / [petr4](p4/status/petr4/run-v1model.analysis.md)

### Contributing

p4cherry is an open-source project. Please feel free to contribute by opening issues or pull requests.

### License

p4cherry is released under the [Apache 2.0 license](LICENSE).
