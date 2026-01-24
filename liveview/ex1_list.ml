open Liveview
open Bonesai.Let_syntax
module List0 = Bonesai.List0

let main _req graph =
  (* TODO augment example to allow deletion of specific elements & reordering.

     We'll need dynamic event handlers for that. One delete event per list item.
   *)
  let list, to_task = List0.create ~start:(List.init 3 Fun.id) graph in
  let list = List0.value list in
  let to_task =
    let+ list = list and+ to_task = to_task in
    function
    | `Add ->
        let n = List.length list in
        to_task List0.(Patch [ I (0, n) ])
    | `Del ->
        (* TODO handle invalid actions in Bonesai itself. Avoid exceptions *)
        if List.length list > 0 then to_task List0.(Patch [ R 0 ])
        else Bonesai.do_nothing
  in
  let add = event_handler to_task `Add graph
  and del = event_handler to_task `Del graph
  and render list add delete =
    let size = List.length list in
    let open Html in
    let button ?(disable = false) label_ action =
      button
        ~a:(a_onclick action :: (if disable then [ a_disabled () ] else []))
        [ txt label_ ]
    in
    [
      button ~disable:(size < 1) "-1" delete;
      txt (Int.to_string size);
      button "+1" add;
    ]
    @ List.concat_map
        (fun i -> [ br (); txt (Printf.sprintf "item %d" i) ])
        list
  in
  Component.(arg3 div) list add del render graph

(* TODO add API that allows to have state machines / components in the incremental container *)

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
