open Bonesai.Let_syntax

module Counter = struct
  open Liveview

  module Action = struct
    type t = Increment | Decrement
  end

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match (action : Action.t) with
    | Increment -> model + 1
    | Decrement -> model - 1

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let+ state = state
    and+ inject = inject
    and+ id = Liveview.component_id ctx graph
    (* TODO implicit identification of components and handlers *)
    and+ incr = Liveview.handler_id ctx graph
    and+ decr = Liveview.handler_id ctx graph in
    let render ctx =
      let open Html in
      let button label_ action id =
        let handler = (id, fun () -> inject action) in
        button ~a:[ a_onclick ctx handler ] [ txt label_ ]
      in
      [
        button "-1" Action.Decrement decr;
        txt (Int.to_string state);
        button "+1" Action.Increment incr;
      ]
    in
    Component.div id render ctx
end

module Input = struct
  open Liveview

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start
        ~apply_action:(fun (_ : _ Bonesai.Apply_action_context.t) _old new_ ->
          new_)
    in
    let+ state = state
    and+ inject = inject
    and+ id = Liveview.component_id ctx graph
    (* TODO implicit identification of components and handlers *)
    and+ upd = Liveview.handler_id ctx graph in
    let handler = (upd, inject) in
    let _ = Bonesai.identify_node graph in
    let render ctx =
      let open Html in
      [
        form
          [
            input
              ~a:[ a_input_type `Text; a_oninput ctx handler; a_value state ]
              ();
          ];
        txt state;
      ]
    in
    Component.div id render ctx
end

let main ~n1 ~n2 ~n3 ~s ctx graph =
  let open Liveview in
  let cutoff c =
    (* TODO I think this should apply to all sub_components; the API should do
       it automatically to avoid redundant updates of the parent component.
       Exception maybe: components that return a value to be used in other
       components. *)
    Bonesai.cutoff ~equal:(fun _ _ -> true) c
  in
  (* TODO demonstrate how to use state across components *)
  let+ one = Counter.component ~start:n1 ctx graph |> cutoff
  and+ two = Counter.component ~start:n2 ctx graph |> cutoff
  and+ three = Counter.component ~start:n3 ctx graph |> cutoff
  and+ four = Input.component ~start:s ctx graph |> cutoff
  and+ id = Liveview.component_id ctx graph in
  let open Html in
  let render ctx =
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
  Component.div id render ctx

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
