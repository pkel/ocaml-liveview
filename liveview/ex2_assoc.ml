open Liveview
open Bonesai.Let_syntax
module IntMap = Map.Make (Int)
module BIntMap = Bonesai.Map1 (IntMap)

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
        txt ("value: " ^ state);
      ]
    in
    Component.(arg2 div) state update render graph
end

let main _req graph =
  (* TODO augment example to allow deletion of specific elements & reordering.

     We'll need dynamic event handlers for that. One delete event per list item.
   *)
  let input key =
    let start = Printf.sprintf "element %d" key in
    Input.component ~start
  in
  let bmap, to_task =
    BIntMap.create ~start:(IntMap.of_list [ (1, input 1); (2, input 2) ]) graph
  in
  let map = BIntMap.value bmap in
  let to_task =
    let+ map = map and+ to_task = to_task in
    function
    | `Add ->
        let key =
          match IntMap.max_binding_opt map with
          | None -> 1
          | Some (key, _) -> key + 1
        in
        to_task BIntMap.(Set (key, input key))
    | `Del key -> (
        (* TODO handle invalid actions in Bonesai itself. Avoid exceptions *)
        match IntMap.find_opt key map with
        | None -> Bonesai.do_nothing
        | Some _ -> to_task BIntMap.(Del key))
  in
  let add = event_handler to_task `Add graph
  and render map add =
    Html.[ button ~a:[ a_onclick add ] [ txt "new input" ] ]
    @ (IntMap.bindings map
      |> List.concat_map (fun (key, el) ->
          Html.
            [ hr (); txt (Printf.sprintf "map-key: %d" key); sub_component el ])
      )
  in
  Component.(arg2 div) map add render graph

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
