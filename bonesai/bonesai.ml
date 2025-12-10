module Effect = struct
  type 'a t = unit -> 'a

  let execute t = t ()
end

module Graph : sig
  type t
end = struct
  type t = unit
end

type 'a t = 'a Value.t
type 'a effect = 'a Effect.t
type graph = Graph.t

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

module Apply_action_context = struct
  type ('action, 'response) t =
    { inject : 'action -> 'response effect
    ; schedule_event : unit effect -> unit
    }

  let inject t = t.inject
  let schedule_event t = t.schedule_event
end

(*
val state_machine
  :  ?equal:('model -> 'model -> bool)
  -> default_model:'model
  -> apply_action:(('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model)
  -> graph
  -> 'model t * ('action -> unit effect) t
  *)

let state_machine ?(equal=(==)) ~default_model ~apply_action _graph =
  let signal, setter = React.S.create ~eq:equal default_model in
  let inject_effect action () =
    let _ = action in
    (* TODO do something useful with [action] *)
    (* But what exactly? At first glance, we could do something similar
       to the apply_effect below but with a fixed action.

       Thinking a bit further, it seems [Effect.t] has to provide the glue
       between GUI and runtime. The naive thing might work initially, but
       what if you want to ship the app as HTML but have the execution happen
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

       Also, while browsing the docs: Do we need client side effects with
       return values in the liveview model? E.g. call local API, send result to
       server? Does that fit into the Ui_effect model?

       *)
    assert false
  in
  let ctx = Apply_action_context.{
    inject = inject_effect;
    schedule_event = Effect.execute;
  }
  in
  let apply_effect action () =
    let old_model = React.S.value signal in
    let new_model = apply_action ctx old_model action in
    setter new_model
  in
  Value.Signal signal, Value.Constant apply_effect
