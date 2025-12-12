module Graph : sig
  type t
end = struct
  type t = unit
end

type 'a t = 'a Value.t
type graph = Graph.t

let return x = Value.Constant x

let map v ~f = Value.Map { t=v; f }
let map2 v1 v2 ~f = Value.Map2 { t1=v1; t2=v2; f }
let both a b = Value.Both (a, b)
let cutoff v ~equal = Value.Cutoff { equal; t = v }

module Effect : sig
  type 'a t

  val create: (unit -> 'a) -> 'a t

  val execute: 'a t -> 'a
end = struct
  type 'a t = unit -> 'a

  let create t = t

  let execute t = t ()

  (* Thinking a bit, it seems [Effect.t] has to provide the glue
     between GUI and runtime. This naive thing might work initially, but
     what if we want to ship the app as HTML but have the execution happen
     on the backend (as in liveview)? The [Effect.t] has to be serialized (at
     least some identifier of it) into HTML, will be sent to the server; and
     has to be interpreted in the right context there.

     Wait, how did I get working my current liveview implementation with
     original Bonsai?

     Bonsai has 'a Effect.t = 'a Virtual_dom.Ui_effect.t

     https://ocaml.org/p/virtual_dom/v0.17.0/doc/virtual_dom.ui_effect/Ui_effect/index.html

     Okay. It seems one can provide own 'action Effect.t if one provides a
     handler of type ('action -> unit).

     But these Ui_effects do more. They may also represent computation that
     happens outside the Bonsai control loop. E.g. requests to an API.
  *)
end

type 'a effect = 'a Effect.t

let state ?(equal = (==)) start _graph =
  let signal, setter = React.S.create ~eq:equal start in
  let set_effect new_model = Effect.create (fun () -> setter new_model) in
  Value.Signal signal, Value.Constant set_effect

let state_opt ?(equal = (==)) ?(default_model) _graph =
  let equal a b = match a, b with
  | Some a, Some b -> equal a b
  | _ -> false
  in
  state ~equal default_model _graph

let state' ?(equal = (==)) start _graph =
  let signal, setter = React.S.create ~eq:equal start in
  let set_effect update = Effect.create (fun () ->
    let old_model = React.S.value signal in
    setter (update old_model) )
  in
  Value.Signal signal, Value.Constant set_effect

let toggle ~default_model _graph =
  let signal, setter = React.S.create ~eq:(==) default_model in
  let toggle_effect = Effect.create (fun () ->
    let old_model = React.S.value signal in
    setter (not old_model))
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
  let set_effect new_model = Effect.create (fun () -> setter new_model) in
  let toggle_effect = Effect.create (fun () ->
    let old_model = React.S.value signal in
    setter (not old_model))
  in
  Toggle.{ state = Value.Signal signal
         ; set_state = Value.Constant set_effect
         ; toggle = Value.Constant toggle_effect
         }

module Apply_action_context = struct
  type ('action, 'response) t =
    { inject : 'action -> 'response effect
    ; schedule_event : unit effect -> unit
    }

  let inject t = t.inject
  let schedule_event t = t.schedule_event
end

let state_machine ?(equal=(==)) ~default_model ~apply_action _graph =
  let signal, setter = React.S.create ~eq:equal default_model in
  let rec ctx = Apply_action_context.{
    inject = apply_effect;
    schedule_event = Effect.execute;
  }
  and apply_effect action = Effect.create (fun () ->
    let old_model = React.S.value signal in
    let new_model = apply_action ctx old_model action in
    setter new_model
  )
  in
  Value.Signal signal, Value.Constant apply_effect
