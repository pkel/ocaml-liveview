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

    val compile : (graph -> 'a value) -> 'a app * extra
    val schedule_effect : 'a app -> unit effect_ -> unit
    val observe : 'a app -> 'a
  end
end

module NoExtra = struct
  type t = unit

  let create () = ()
end

module Make (Effect : Effect) (Extra : Extra) :
  WithExtra with type extra = Extra.t and type 'a effect_ = 'a Effect.t = struct
  type 'a effect_ = 'a Effect.t
  type extra = Extra.t
  type graph = Extra.t (* think: unit plus some opaque value *)

  let extra graph = graph

  type 'a value = 'a React.signal

  let phys_equal = ( == )
  let return = React.S.const
  let map a ~f = React.S.l1 ~eq:phys_equal f a
  let map2 a b ~f = React.S.l2 ~eq:phys_equal f a b
  let both a b = map2 a b ~f:(fun a b -> (a, b))

  module Let_syntax = struct
    let ( let+ ) v f = map v ~f
    let ( and+ ) = both
  end

  let cutoff s ~equal =
    (* In principle, all React.S.* functions support an ~eq parameter to
       integrate the cutoff directly into the node. We here do it differently
       and allocate a new node with the requested cutoff.

       I had some machinery to optimize this until commit 13bb591bf but dropped
       it for simplicity's sake.
    *)
    React.S.map ~eq:equal Fun.id s

  module Runtime = struct
    type 'a app = 'a React.signal

    let compile bonesai =
      let graph = Extra.create () in
      let signal = bonesai graph in
      (signal, graph (* graph = extra *))

    let observe = React.S.value
    let schedule_effect _app = Effect.execute
  end

  let signal s = s
  let constant x = return x

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
    type t = {
      state : bool value;
      set_state : (bool -> unit effect_) value;
      toggle : unit effect_ value;
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
      inject : 'action -> 'response effect_;
      schedule_event : unit effect_ -> unit;
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

  let actor ?(equal = ( == )) ~default_model ~recv _graph =
    let s, setter = React.S.create ~eq:equal default_model in
    let rec ctx =
      Apply_action_context.
        { inject = apply_effect; schedule_event = Effect.execute }
    and apply_effect action =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          let new_model, return = recv ctx old_model action in
          let () = setter new_model in
          return)
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
          let input = React.S.value input in
          let new_model = apply_action ctx input old_model action in
          setter new_model)
    in
    (signal s, constant apply_effect)

  let actor_with_input ?(equal = ( == )) ~default_model ~recv input _graph =
    let s, setter = React.S.create ~eq:equal default_model in
    let rec ctx =
      Apply_action_context.
        { inject = apply_effect; schedule_event = Effect.execute }
    and apply_effect action =
      Effect.create (fun () ->
          let old_model = React.S.value s in
          let input = React.S.value input in
          let new_model, return = recv ctx input old_model action in
          let () = setter new_model in
          return)
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
