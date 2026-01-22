open Liveview
(* open Bonesai.Let_syntax *)

module BList = Bonesai.BList

let main _req graph =
  (* TODO augment example to allow deletion of specific elements & reordering *)
  let list, to_task = BList.create ~start:[ (); (); () ] graph in
  let incr = handler to_task BList.(Patch [ I (0, ()) ]) graph
  and decr = handler to_task BList.(Patch [ R 0 ]) graph
  and render list incr decr =
    let size = List.length list in
    let open Html in
    let button label_ action = button ~a:[ a_onclick action ] [ txt label_ ] in
    [ button "-1" decr; txt (Int.to_string size); button "+1" incr ]
    @ List.concat_map (fun () -> [ br (); txt "item ()" ]) list
  in
  Component.(arg3 div) (BList.value list) incr decr render graph

(* TODO add API that allows to have state machines / components in the incremental container *)

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions @@ dream main
