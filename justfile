dependencies:
  dune build *.opam
  opam install . --deps-only --with-test --with-dev-setup
