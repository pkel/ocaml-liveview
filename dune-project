(lang dune 3.14)

(name liveview)

(generate_opam_files true)

(package
  (name liveview)
  (synopsis "Experiments to build a liveview and bonsai inspired web framework")
  (depends
    (ocaml (= 5.1.1))
    (dune :build)
    react
    reactiveData
    tyxml
    (dream (= "1.0.0~alpha8"))
    (lwt_ppx (< 6))
    (yojson (and (>= 3.0.0) (< 4)))
    ppx_yojson_conv ; also recursive dependency of dream
    (ppx_yojson_conv_lib :with-dev-setup)
    (crunch (and (>= 4.0.0) (< 5) :with-dev-setup))
    (alcotest :with-test)
    (testo :with-test)
    (odig :with-dev-setup)
    (odoc :with-dev-setup)
    (ocamlformat (and :with-dev-setup (= 0.28.1)))
  ))
