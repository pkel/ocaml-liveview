open Liveview
open Bonesai.Let_syntax

module Counter = struct
  type action = Incr | Decr

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match action with Incr -> model + 1 | Decr -> model - 1

  let component ~start graph =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let incr = handler inject Incr graph
    and decr = handler inject Decr graph
    and render state incr decr =
      let open Html in
      let button label_ action =
        button ~a:[ a_onclick action ] [ txt label_ ]
      in
      [ button "-1" decr; txt (Int.to_string state); button "+1" incr ]
    in
    (Component.(arg3 div) state incr decr render graph, state)
end

module Input = struct
  type action = Update of string

  let update string = Update string

  let apply_action (_ : _ Bonesai.Apply_action_context.t) _old = function
    | Update new_ -> new_

  let component ~start graph =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let update = handler' inject string update graph
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

let main _req graph =
  (* TODO make this component more like a list where you can add input elements
     and remove them individually. *)
  let input _i _v graph =
    (* TODO build input component that can take start value from _i Bonesai.t *)
    Input.component ~start:"" graph
  in
  let counter, n = Counter.component ~start:3 graph in
  let placeholders =
    let+ n = n in
    List.init n (fun i -> (i, ())) |> Bonesai.IntMap.of_list
  in
  let inputs = Bonesai.assoc_int input placeholders graph in
  let render counter inputs =
    let open Html in
    [ sub_component counter; hr () ]
    @ Bonesai.IntMap.fold
        (fun _key c acc -> sub_component c :: hr () :: acc)
        inputs []
  in
  Component.(arg2 div) counter inputs render graph

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
