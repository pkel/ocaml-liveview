type state = Counter of int

let init () = Counter 42

type action = Incr | Decr

let apply (Counter x) = function
  | Incr -> Counter (x + 1)
  | Decr -> Counter (x - 1)

let render (Counter x) =
  let open Tyxml.Html in
  div [
    p [txt (string_of_int x)];
    form ~a:[a_action "."; a_method `Post] [
      input ~a:[a_input_type `Submit; a_formaction ".?action=incr"; a_value "Incr"] ();
      input ~a:[a_input_type `Submit; a_formaction ".?action=decr"; a_value "Decr"] ()
      ]
  ]

(*
  above: application
  below: framework
*)

let action_of_string = function
  | "incr" -> Some Incr
  | "decr" -> Some Decr
  | _ -> None

let dream_tyxml x =
  let str = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) x in
  Dream.html str

let get_state req =
  match Dream.session_field req "state" with
  | None -> init ()
  | Some str -> Marshal.from_string str 0

let put_state req state =
  Dream.set_session_field req "state" (Marshal.to_string state [])

let action_handler req action =
  match action_of_string action with
    | Some action ->
        let state = get_state req in
        let state' = apply state action in
        let%lwt () = put_state req state' in
        Dream.redirect req "."
    | None ->
        Dream.redirect req "."

let get_handler req =
  let state = get_state req in
  dream_tyxml (render state)

let handler req =
  match Dream.query req "action" with
  | Some str -> action_handler req str
  | None -> get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
