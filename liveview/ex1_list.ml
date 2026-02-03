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
        to_task List0.(Patch [I (0, n)])
    | `Del ->
        (* TODO handle invalid actions in Bonesai itself. Avoid exceptions *)
        if List.length list > 0 then to_task List0.(Patch [R 0])
        else Bonesai.do_nothing
  in
  let size = Bonesai.map ~f:List.length list in
  let open Html in
  [%component
    div
      ( [ button
            ~a:[(if [%v size] < 1 then a_disabled () else a_onclick [%a `Del])]
            [txt "+1"]
        ; txt (Int.to_string [%v size])
        ; button ~a:[a_onclick [%a `Add]] [txt "+1"] ]
      @ List.concat_map
          (fun i -> [br (); txt (Printf.sprintf "item %d" i)])
          [%v list] )]

(* TODO add API that allows to have state machines / components in the incremental container *)

let template scripts app =
  let open Html in
  html (head (title (txt "Liveview - ex1_list")) scripts) (body [app])

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router (dream template main)
