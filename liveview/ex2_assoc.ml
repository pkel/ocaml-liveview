open Liveview
open Bonesai.Let_syntax
module IntMap = LMap (Int)

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
  let input key graph =
    let start = Printf.sprintf "element %d" key in
    (Bonesai.return (), Input.component ~start graph)
  in
  let map, to_task, rendered =
    IntMap.create Component.div
      ~start:(IntMap.of_list [(1, input 1); (2, input 2)])
      graph
  in
  let to_task =
    let+ map = map and+ to_task = to_task in
    function
    | `Add ->
        let key =
          match IntMap.max_binding_opt map with
          | None ->
              1
          | Some (key, _) ->
              key + 1
        in
        to_task IntMap.(Set (key, input key))
    | `Del key -> (
      (* TODO handle invalid actions in Bonesai/Liveview itself. Avoid exceptions *)
      match IntMap.find_opt key map with
      | None ->
          Bonesai.do_nothing
      | Some _ ->
          to_task IntMap.(Del key) )
  in
  [%component
    div
      [ button ~a:[a_onclick [%a `Add]] [txt "new input"]
      ; sub_component [%v rendered] ]]

let template scripts app =
  let open Html in
  html (head (title (txt "Liveview - ex2_assoc")) scripts) (body [app])

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router (dream template main)
