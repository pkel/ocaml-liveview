open Liveview

module Counter = struct
  type action = Incr | Decr

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match action with Incr -> model + 1 | Decr -> model - 1

  let component ~start graph =
    let state, to_task =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    [%component
      div
        [ button ~a:[a_onclick [%a Decr]] [txt "-1"]
        ; txt (Int.to_string [%v state])
        ; button ~a:[a_onclick [%a Incr]] [txt "+1"] ]]
end

module Input = struct
  type action = Update of string

  let update string = Update string

  let apply_action (_ : _ Bonesai.Apply_action_context.t) _old = function
    | Update new_ ->
        new_

  let component ~start graph =
    let state, to_task =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    [%component
      div (* TODO It might be cool to add attributes with ~a here *)
        [ form (* TODO I think form could be a top-level component type *)
            [ input
                ~a:
                  [ a_input_type `Text
                  ; a_oninput [%astring update]
                  ; a_value [%v state] ]
                () ]
        ; txt [%v state] ]]
end

let main ~n1 ~n2 ~n3 ~s graph =
  let one = Counter.component ~start:n1 graph
  and two = Counter.component ~start:n2 graph
  and three = Counter.component ~start:n3 graph
  and four = Input.component ~start:s graph in
  [%component
    div
      [ sub_component [%v one]
      ; hr ()
      ; sub_component [%v two]
      ; hr ()
      ; sub_component [%v three]
      ; hr ()
      ; sub_component [%v four] ]]

(* read arguments from Dream request *)

let parse_query_s req =
  match Dream.query req "s" with None -> "init" | Some s -> s

let parse_query_n1 req =
  match Dream.query req "n1" with
  | None ->
      42
  | Some x -> (
    match int_of_string_opt x with None -> 42 | Some x -> x )

let parse_query_n2 req =
  match Dream.query req "n2" with
  | None ->
      13
  | Some x -> (
    match int_of_string_opt x with None -> 13 | Some x -> x )

let parse_query_n3 req =
  match Dream.query req "n3" with
  | None ->
      21
  | Some x -> (
    match int_of_string_opt x with None -> 21 | Some x -> x )

let main req =
  let n1 = parse_query_n1 req
  and n2 = parse_query_n2 req
  and n3 = parse_query_n3 req
  and s = parse_query_s req in
  main ~n1 ~n2 ~n3 ~s

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream ~slowdown:0.3 main
