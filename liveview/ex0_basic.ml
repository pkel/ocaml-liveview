open Liveview

module Counter = struct
  type action = Incr | Decr

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match action with Incr -> model + 1 | Decr -> model - 1

  let component ~start graph =
    let state, to_task =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let incr = event_handler to_task Incr graph
    and decr = event_handler to_task Decr graph
    and render state incr decr =
      let open Html in
      let button label_ action =
        button ~a:[ a_onclick action ] [ txt label_ ]
      in
      [ button "-1" decr; txt (Int.to_string state); button "+1" incr ]
    in
    (* TODO can we do some let+ and+ in magic here to avoid all the arguments?
       One thing that would work for certain is PPX. And in a separate step,
       such a PPX could mark all locations, where a value (not a handler) is
       injected into the HTML. This would allow to compile HTML templates and
       update the values individually like it happens in liveview. *)
    Component.(arg3 div) state incr decr render graph
end

module Input = struct
  type action = Update of string

  let update string = Update string

  let apply_action (_ : _ Bonesai.Apply_action_context.t) _old = function
    | Update new_ -> new_

  let component ~start graph =
    let state, to_task =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let update = string_event_handler to_task update graph
    and render state update =
      let open Html in
      [
        form
          [
            input ~a:[ a_input_type `Text; a_oninput update; a_value state ] ();
          ];
        txt state;
      ]
    in
    Component.(arg2 div) state update render graph
end

let main ~n1 ~n2 ~n3 ~s graph =
  (* TODO demonstrate how to use state across components *)
  let one = Counter.component ~start:n1 graph
  and two = Counter.component ~start:n2 graph
  and three = Counter.component ~start:n3 graph
  and four = Input.component ~start:s graph
  and render one two three four =
    let open Html in
    [
      sub_component one;
      hr ();
      sub_component two;
      hr ();
      sub_component three;
      hr ();
      sub_component four;
    ]
  in
  Component.(arg4 div) one two three four render graph

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

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
