open Liveview
open Bonesai.Let_syntax
module IntMap = Map.Make (Int)
module BIntMap = Bonesai.Map1 (IntMap)

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
      div
        [ form
            [ input
                ~a:
                  [ a_input_type `Text
                  ; a_oninput [%astring update]
                  ; a_value [%v state] ]
                () ]
        ; txt ("value: " ^ [%v state]) ]]
end

let main _req graph =
  (* TODO augment example to allow deletion of specific elements & reordering.

     We'll need dynamic event handlers for that. One delete event per list item.
   *)
  let input key =
    let start = Printf.sprintf "element %d" key in
    Input.component ~start
  in
  let bimap, to_task =
    BIntMap.create ~start:(IntMap.of_list [(1, input 1); (2, input 2)]) graph
  in
  (* TODO can we eliminate the difference between bimap and imap? *)
  let imap = BIntMap.value bimap in
  let to_task =
    let+ imap = imap and+ to_task = to_task in
    function
    | `Add ->
        let key =
          match IntMap.max_binding_opt imap with
          | None ->
              1
          | Some (key, _) ->
              key + 1
        in
        to_task BIntMap.(Set (key, input key))
    | `Del key -> (
      (* TODO handle invalid actions in Bonesai itself. Avoid exceptions *)
      match IntMap.find_opt key imap with
      | None ->
          Bonesai.do_nothing
      | Some _ ->
          to_task BIntMap.(Del key) )
  in
  [%component
    div
      ( [button ~a:[a_onclick [%a `Add]] [txt "new input"]]
      @ ( IntMap.bindings [%v imap]
        (* The pattern, mapping the map to a list of elements will happen
           regularly, if not always. TODO provide helper for that. Also see my
           note on how React handles this in bonesai.impl/Map1. Maybe limiting
           the expressiveness is fine; provide one definite way of rendering a
           keyed assoc/map into a component with one children per elements. *)
        |> List.concat_map (fun (key, el) ->
            [hr (); txt (Printf.sprintf "map-key: %d" key); sub_component el] )
        ) )]

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
