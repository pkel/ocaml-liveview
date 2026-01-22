let tests0 =
  [
    Testo.create "zero" (fun () -> ());
    Testo.create "one"
      (fun () -> assert false)
      ~expected_outcome:(Should_fail "assertion");
  ]

let counter start_state graph =
  let state, to_task =
    Bonesai.state_machine ~default_model:start_state
      ~apply_action:(fun _ctx ctr -> function
        | `Incr -> ctr + 1 | `Decr -> ctr - 1)
      graph
  in
  Bonesai.both state to_task

(* TODO test Bonsai.{state, state_opt, state', toggle, toggle'} *)

let tests1 =
  [
    Testo.create "counter" (fun () ->
        let open Bonesai.Runtime in
        let app = compile (counter 7) in
        let to_task action =
          let _, fn = observe app in
          schedule app (fn action)
        and observe () =
          let state, _ = observe app in
          state
        in
        Alcotest.(check int) "init" 7 (observe ());
        to_task `Incr;
        Alcotest.(check int) "incr" 8 (observe ());
        to_task `Decr;
        Alcotest.(check int) "decr 1st" 7 (observe ());
        to_task `Decr;
        Alcotest.(check int) "decr 2nd" 6 (observe ()));
  ]

let () =
  Testo.interpret_argv ~project_name:"bonesai" (fun _env -> tests0 @ tests1)
