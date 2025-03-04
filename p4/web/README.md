# P4-Sandbox

A web-based playground for P4

## Building

### Prerequisites for P4 Sandbox

Install required libraries for compilation.

```shell
$ opam switch 4.14.0
$ eval $(opam env)
$ opam install yojson js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt promise_jsoo_lwt.0.4.2
```

### Building the Project

```shell
$ make web
```

This creates `web.bc.js` in `p4cherry/p4/web/html_build`.

Then open [`p4cherry/p4/web/html_build/index.html`](./html_build/index.html) in a web browser.
