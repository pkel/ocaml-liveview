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

  let state_machine_with_input ?(equal = ( == )) ~default_model ~apply_action
      input _graph =
    let s, setter = React.S.create ~eq:equal default_model in
    let rec ctx =
      Apply_action_context.
        { inject = apply_effect; schedule_event = Effect.execute }
    and apply_effect action =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          let input =
            (* TODO avoid redundant Value.eval

               This is an interesting case, actually. The Value.eval would
               construct an entirely new React graph instead of attaching the
               existing input graph to the new state machine. So, this TODO is
               quite important.

               But how to solve it? My Value.t does not allow it. And in JS
               Bonsai.private_base.value.t it's not obvious. Maybe the [Named]
               constructor is the key? Similarly, what happens in
               private_base/computation.ml and Trampoline.t?

               Is there maybe a 80/20 solution that avoids additional
               machinery. Reduce existing machinery, even? E.g. can we maybe
               skip creating the Value.t ADT representation and instead build
               React.signals directly?

               But let's first implement the JS Bonsai.{actor, wrap} functions.
               My suspicion is, wrap has the same problem.
               Bonsai.actor_with_input certainly has it.
               *)
            React.S.value (Value.eval input)
          in
          let new_model = apply_action ctx input old_model action in
          setter new_model)
    in
    (signal s, constant apply_effect)

  module BData (RData : ReactiveData.S) = struct
    type 'a patch = 'a RData.patch
    type 'a raw = 'a RData.data
    type 'a action = Patch of 'a patch | Set of 'a raw
    type 'a data = 'a RData.t

    let create ~start (_ : graph) =
      let data, handle = RData.create start in
      let inject = function
        | Patch p -> Effect.create (fun () -> RData.patch handle p)
        | Set d -> Effect.create (fun () -> RData.set handle d)
      in
      (data, constant inject)

    let value data =
      (* TODO this would've an ~eq argument which we do not expose *)
      signal (RData.signal data)
  end

  module BList = struct
    module RList = ReactiveData.RList

    type 'a p = 'a RList.p =
      | I of int * 'a
      | R of int
      | U of int * 'a
      | X of int * int

    include BData (RList)
  end

  module BMap (M : Map.S) = struct
    module RMap = ReactiveData.RMap (M)
    include BData (RMap)

    let filter = RMap.filter
  end
end
