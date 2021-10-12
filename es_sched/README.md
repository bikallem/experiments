# OCaml effects based examples using effects syntax

## Preparse OCaml switch:

```
opam switch create 4.12.0+domains+effects --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
opam pin add -yn ppxlib 0.22.0+effect-syntax
opam pin add -yn ocaml-migrate-parsetree 2.1.0+effect-syntax
opam install dune merlin utop
```
## Run

`dune exec ./es_sched.exe`
