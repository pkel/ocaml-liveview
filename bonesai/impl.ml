module type Effect = sig
  (** Effects are opaque boxes of future computations. Consumers of the Bonesai
      API never create or execute effects manually. Experts can instantiate
      Bonesai with their own effect type. *)

  type 'a t

  val create : (unit -> 'a) -> 'a t
  val execute : 'a t -> 'a
end

module Effect : Effect = struct
  type 'a t = unit -> 'a

  let create f = f
  let execute f = f ()
end

(* Consumers might assume the graph argument holds some complex state. Experts
 * know, the graph is just type unit. This interface allows experts to add some
 * additional context info to the graph type. *)

module type Extra = sig
  type t

  val create : unit -> t
end

module type WithExtra = sig
  include Intf.Bonesai

  type extra

  val extra : graph -> extra

  module Runtime : sig
    type 'a app

    val compile : (graph -> 'a t) -> 'a app * extra
    val schedule_effect : 'a app -> unit effect -> unit
    val observe : 'a app -> 'a
  end
end

module NoExtra = struct
  type t = unit

  let create () = ()
end

module Make (Effect : Effect) (Extra : Extra) :
  WithExtra with type extra = Extra.t and type 'a effect = 'a Effect.t = struct
  type 'a effect = 'a Effect.t
  type extra = Extra.t
  type graph = Extra.t (* think: unit plus some opaque value *)

  let extra graph = graph

  type 'a t = 'a Value.t

  let return = Value.constant
  let map = Value.map
  let map2 = Value.map2
  let both = Value.both

  module Let_syntax = struct
    let ( let+ ) v f = map v ~f
    let ( and+ ) = both
  end

  let cutoff = Value.cutoff

  module Runtime = struct
    type 'a app = 'a React.signal

    let compile bonesai =
      let graph = Extra.create () in
      let value = bonesai graph in
      (Value.eval value, graph)

    let observe = React.S.value
    let schedule_effect _app = Effect.execute
  end

  let signal = Value.signal
  let constant = Value.constant

  let state ?(equal = ( == )) start _graph =
    let s, setter = React.S.create ~eq:equal start in
    let set_effect new_model = Effect.create (fun () -> setter new_model) in
    (signal s, constant set_effect)

  let state_opt ?(equal = ( == )) ?default_model _graph =
    let equal a b =
      match (a, b) with Some a, Some b -> equal a b | _ -> false
    in
    state ~equal default_model _graph

  let state' ?(equal = ( == )) start _graph =
    let s, setter = React.S.create ~eq:equal start in
    let set_effect update =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          setter (update old_model))
    in
    (signal s, constant set_effect)

  let toggle ~default_model _graph =
    let s, setter = React.S.create ~eq:( == ) default_model in
    let toggle_effect =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          setter (not old_model))
    in
    (signal s, constant toggle_effect)

  module Toggle = struct
    type nonrec t = {
      state : bool t;
      set_state : (bool -> unit effect) t;
      toggle : unit effect t;
    }
  end

  let toggle' ~default_model _graph =
    let s, setter = React.S.create ~eq:( == ) default_model in
    let set_effect new_model = Effect.create (fun () -> setter new_model) in
    let toggle_effect =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          setter (not old_model))
    in
    Toggle.
      {
        state = signal s;
        set_state = constant set_effect;
        toggle = constant toggle_effect;
      }

  module Apply_action_context = struct
    type ('action, 'response) t = {
      inject : 'action -> 'response effect;
      schedule_event : unit effect -> unit;
    }

    let inject t = t.inject
    let schedule_event t = t.schedule_event
  end

  let state_machine ?(equal = ( == )) ~default_model ~apply_action _graph =
    let s, setter = React.S.create ~eq:equal default_model in
    let rec ctx =
      Apply_action_context.
        { inject = apply_effect; schedule_event = Effect.execute }
    and apply_effect action =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          let new_model = apply_action ctx old_model action in
          setter new_model)
    in
    (signal s, constant apply_effect)

  module IntMap = Map.Make (Int)
  module StringMap = Map.Make (String)

  let assoc_int f m graph =
    (* m is a [Map.t value]
     * f is a ['a -> 'b] function
     * We want to build a graph of incremental computations (or reactive signals)
     * that applies f to all elements of [m] incrementally.
     *
     * We have to handle three scenarios:
     * - key is removed in input map: remove key from output map & destruct a part of the graph
     * - key is added to the input map: add key to the output map & construct graph
     * - value for given key is updated: fire incremental update to the respective output values
     *
     * So, to have this, we'll have to categorize all involved keys:
     * - `New keys are in new_map but not in old_map.
     * - `Static keys are in new_map and old_map, the value is equal.
     * - `Updated keys are in new_map and old_map, the value has changed.
     * - `Stale keys are in old_map but not in new_map.
     *
     * Then take action:
     * - `New key: allocate reactive signal, initialize with [f x], add to output map
     * - `Static key: ignore
     * - `Updated key: update reactive signal with [f x]
     * - `Stale key: deallocate reactive signal, remove from output map
     *
     * I'll do this naively first. Diff the entire set of input keys on each
     * update & construct a new output map on each update.
     *
     * With some thought it should be possible to implement an incremental
     * version of the map data structure. One that modifies the compute graph
     * whenever elements are added/set/removed. Or would that require monadic
     * bind?
     *
     * However, I think at the core we need an imperative data structure. The
     * graph construction/destruction is imperative anyway. So should I start
     * with an imperative wrapper, that takes single `New/`Update/`Remove
     * operations and modifies the compute graph in return?
     *
     * See https://ocaml.org/p/cumulus/0.0.1, a signal-like type that supports
     * differential updates of the underlying value.
     *
     * Similarly: https://ocaml.org/p/reactiveData/0.3.1
     *
     * Okay, these two libraries solve the imperative part. ReactiveData seems to
     * be maintained and finds some application in the OCaml ecosystem. Let's go
     * with that for now.
     *)
    ignore (f, m, graph);
    assert false (* TODO assoc_int *)

  let assoc_string = assert false (* TODO assoc_string *)
end
