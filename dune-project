(lang dune 3.14)

(name liveview)

(generate_opam_files true)

(package
  (name liveview)
  (synopsis "Experiments to build a liveview and bonsai inspired web framework")
  (depends
    (ocaml (= 5.1.1))
    (dune :build)
    (bonsai (= v0.17.0)) ; for draft_with_jsbonsai
    react
    reactiveData
    (dream (= "1.0.0~alpha8"))
    (tyxml (= 4.6.0))
    (lwt_ppx (= 5.9.1))
    (yojson (= 3.0.0))
    (ppx_yojson_conv (= v0.17.0)) ; also used in dream
    (ppx_yojson_conv_lib (and (= v0.17.0) :with-dev-setup))
    (crunch (and (= 4.0.0) :with-dev-setup))
    (ppx_expect :with-test)
    (alcotest :with-test)
    (testo :with-test)
    (odig :with-dev-setup)
    (odoc :with-dev-setup)
    (ocamlformat (and :with-dev-setup (= 0.28.1)))
  ))
