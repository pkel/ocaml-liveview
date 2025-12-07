module Effect = struct
  type 'a t = unit -> 'a
end

type 'a t = 'a Value.t
type 'a effect = 'a Effect.t

type graph = Primitives.graph

let return x = Value.Constant x

let map v ~f = Value.Map { t=v; f }
let map2 v1 v2 ~f = Value.Map2 { t1=v1; t2=v2; f }
let both a b = Value.Both (a, b)
let cutoff v ~equal = Value.Cutoff { equal; t = v }

let state ?(equal = (==)) start _graph =
  let signal, setter = React.S.create ~eq:equal start in
  let set_effect new_model () = setter new_model in
  Value.Signal signal, Value.Constant set_effect

let state_opt ?(equal = (==)) ?(default_model) _graph =
  let equal a b = match a, b with
  | Some a, Some b -> equal a b
  | _ -> false
  in
  state ~equal default_model _graph

let state' ?(equal = (==)) start _graph =
  let signal, setter = React.S.create ~eq:equal start in
  let set_effect update () =
    let old_model = React.S.value signal in
    setter (update old_model)
  in
  Value.Signal signal, Value.Constant set_effect

let toggle ~default_model _graph =
  let signal, setter = React.S.create ~eq:(==) default_model in
  let toggle_effect () =
    let old_model = React.S.value signal in
    setter (not old_model)
  in
  Value.Signal signal, Value.Constant toggle_effect

module Toggle = struct
  type nonrec t =
    { state : bool t
    ; set_state : (bool -> unit effect) t
    ; toggle : unit effect t
    }
end

let toggle' ~default_model _graph =
  let signal, setter = React.S.create ~eq:(==) default_model in
  let set_effect new_model () = setter new_model in
  let toggle_effect () =
    let old_model = React.S.value signal in
    setter (not old_model)
  in
  Toggle.{ state = Value.Signal signal
         ; set_state = Value.Constant set_effect
         ; toggle = Value.Constant toggle_effect
         }
