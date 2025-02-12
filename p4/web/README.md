# P4-Sandbox
A playground for P4 programs.

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

### Local server setup

Install `flask` in a python virtual environment.
```shell
cd p4/web
$ python3 -m venv .venv
$ source .venv/bin/activate
$ pip install flask
```

## Running

```shell
$ python3 server.py
```

This opens a local server at `http://localhost:8080/`.

## Cleanup

Deactivate the virtual environment.
```shell
$ deactivate
```

Note that the server internally saves the input p4 program and packet in a /tmp directory. This is a temporary solution and will be changed in the future.

To remove the saved files,
```shell
$ rm /tmp/program.p4
$ rm /tmp/program.stf
```
