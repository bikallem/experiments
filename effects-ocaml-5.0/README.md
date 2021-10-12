# OCaml 5.0 effects samples

## Prepare OCaml switch:

```
opam switch create . 4.12.0+domains --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
opam install dune merlin utop
```
## Run

`dune exec ./sched.exe`
