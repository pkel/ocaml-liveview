dependencies:
  dune build *.opam
  opam install . --deps-only --with-test --with-dev-setup

run-restart path:
  echo _build/default/{{path}} | entr -rdd _build/default/{{path}}
