let tests0 =
  [ Testo.create "zero"
      (fun () -> ())
  ; Testo.create "one"
      (fun () -> assert false)
      ~expected_outcome:(Should_fail "assertion")
  ]

let counter start_state =
  Bonesai.state_machine
    ~default_model:start_state
    ~apply_action:(fun _ctx ctr -> function
      | `Incr -> ctr + 1
      | `Decr -> ctr - 1
    )

(* TODO test Bonsai.{state, state_opt, state', toggle, toggle'} *)

let tests1 =
  [ Testo.create "counter"
      (fun () ->
        let open Bonesai.Runtime in
        let app = init (counter 7) in
        Alcotest.(check int) "init" 7 (observe app);
        inject app `Incr;
        Alcotest.(check int) "incr" 8 (observe app);
        inject app `Decr;
        Alcotest.(check int) "decr 1st" 7 (observe app);
        inject app `Decr;
        Alcotest.(check int) "decr 2nd" 6 (observe app);
        )
  ]

let () =
  Testo.interpret_argv
    ~project_name:"bonesai"
    (fun _env -> tests0 @ tests1)
