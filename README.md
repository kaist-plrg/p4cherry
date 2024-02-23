# P4Cherry

(WIP) A to-be interpreter for P4, cherry-picked from the Petr4 repository.

## Building

### Prerequisites

You will need `ocaml` installed with `dune` and `menhir` library using `opam`.

* Install `opam` version 2.0.5 or higher.
  ```
  $ apt-get install opam
  $ opam init
  ```

* Set `ocaml` as version 4.14.0.
  ```
  $ opam switch create 4.14.0
  ```
  
* Install `dune` version 3.13.0 and `menhir` version 20231231 via `opam`.
  ```
  $ opam install dune menhir
  ```

### Building the Project

```shell
$ dune build
```
