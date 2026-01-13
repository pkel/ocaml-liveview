module Counter = struct
  open Liveview

  type action = Incr | Decr

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match action with Incr -> model + 1 | Decr -> model - 1

  let component ~start ctx graph =
    (* TODO we're passing a lot of ctx graph arguments around, can we eliminate
       some? Maybe merge ctx+graph? They seem to serve a similar purpose *)
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let incr = Liveview.handler inject Incr ctx graph
    and decr = Liveview.handler inject Decr ctx graph
    and render state incr decr ctx =
      let open Html in
      let button label_ action =
        button ~a:[ a_onclick ctx action ] [ txt label_ ]
      in
      [ button "-1" decr; txt (Int.to_string state); button "+1" incr ]
    in
    (* TODO can we do some let+ and+ in magic here to avoid all the arguments? *)
    Component.(arg3 div) state incr decr render ctx graph
end

module Input = struct
  open Liveview

  type action = Update of string

  let update string = Update string

  let apply_action (_ : _ Bonesai.Apply_action_context.t) _old = function
    | Update new_ -> new_

  let component ~start ctx graph =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let update = Liveview.handler' inject string update ctx graph
    and render state update ctx =
      let open Html in
      [
        form
          [
            input
              ~a:[ a_input_type `Text; a_oninput ctx update; a_value state ]
              ();
          ];
        txt state;
      ]
    in
    Component.(arg2 div) state update render ctx graph
end

let main ~n1 ~n2 ~n3 ~s ctx graph =
  let open Liveview in
  (* TODO demonstrate how to use state across components *)
  let one = Counter.component ~start:n1 ctx graph
  and two = Counter.component ~start:n2 ctx graph
  and three = Counter.component ~start:n3 ctx graph
  and four = Input.component ~start:s ctx graph
  and render one two three four ctx =
    let open Html in
    [
      sub_component ctx one;
      hr ();
      sub_component ctx two;
      hr ();
      sub_component ctx three;
      hr ();
      sub_component ctx four;
    ]
  in
  Component.(arg4 div) one two three four render ctx graph

(* read arguments from Dream request *)

let parse_query_s req =
  match Dream.query req "s" with None -> "init" | Some s -> s

let parse_query_n1 req =
  match Dream.query req "n1" with
  | None -> 42
  | Some x -> ( match int_of_string_opt x with None -> 42 | Some x -> x)

let parse_query_n2 req =
  match Dream.query req "n2" with
  | None -> 13
  | Some x -> ( match int_of_string_opt x with None -> 13 | Some x -> x)

let parse_query_n3 req =
  match Dream.query req "n3" with
  | None -> 21
  | Some x -> ( match int_of_string_opt x with None -> 21 | Some x -> x)

let main req =
  let n1 = parse_query_n1 req
  and n2 = parse_query_n2 req
  and n3 = parse_query_n3 req
  and s = parse_query_s req in
  main ~n1 ~n2 ~n3 ~s

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Liveview.Dream.handler main
