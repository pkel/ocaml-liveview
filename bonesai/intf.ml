module type Bonesai = sig
  (** Adapted from Janestreet's Bonsai library, late 2025 / early 2026.
      https://github.com/janestreet/bonsai/blob/8e6c34dceff46c92c3db58d7801da805417139ad/src/cont.mli
  *)

  type 'a t
  (** The primary type in the Bonesai library, you can think of a value with
      type ['a t] as "an 'a that changes over time". The two main ways that you
      get a value with this type are:

      1. by creating a state machine
      - the current value of the state machine is returned as a
        ['state Bonesai.t]
      - the "send the state machine an action" function is also inside of a
        [Bonesai.t]

      2. by mapping on existing [Bonesai.t]'s to derive a new computed
      [Bonesai.t] *)

  type 'a effect

  type graph
  (** [Bonesai.graph] is a required parameter to all Bonesai functions which do
      more than pure computation. The value is always [local_] because Bonesai
      applications have two phases:

      1. The graph building phase. This is the phase where you have access to a
      [local_ Bonesai.graph]

      2. Runtime. The application has started and modifying the graph is no
      longer permitted. *)

  val return : 'a -> 'a t
  (** [return] produces a [Bonesai.t] whose inner value is constant. *)

  val map : 'a t -> f:('a -> 'b) -> 'b t
  (** [map], [map2], and [both] are ways to build a new [Bonesai.t] which is
      dependent on the values of other [Bonesai.t]. As noted above, you should
      prefer to use [let%arr] than these functions, because they come with some
      performance benefits. *)

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val both : 'a t -> 'b t -> ('a * 'b) t

  module Let_syntax : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  val cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t
  (** The Bonesai runtime will recompute a node if any of its dependencies
      change. But sometimes, you may want to consider two contained values to be
      "close enough" and cut off recomputation. You can do that by passing a
      custom equality function to [Bonesai.cutoff]. *)

  val state :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model t * ('model -> unit effect) t
  (** [Bonesai.state] allocates a stateful Bonesai.t node in the graph. It
      returns both the [Bonesai.t] containing the current state, as well as a
      [Bonesai.t] containing a function for overwriting the state.

      You must provide a "starting" value for the state.

      [?equal] (default [phys_equal]) is used by some combinators to reduce
      memory usage. (E.g. [assoc] and [match%sub] may determine that they can
      store a reference to only the default model rather than the current
      model.) *)

  val state_opt :
    ?equal:('model -> 'model -> bool) ->
    ?default_model:'model ->
    graph ->
    'model option t * ('model option -> unit effect) t
  (** [state_opt] is just like [state] except that the model is optional. The
      model starts out as [None] unless you provide a value to the
      [default_model] optional parameter. *)

  val state' :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model t * (('model -> 'model) -> unit effect) t
  (** Similar to [state], but the `set` function takes a function that
      calculates the new state from the previous state. *)

  val toggle : default_model:bool -> graph -> bool t * unit effect t
  (** [Bonesai.toggle] is a small helper function for building a [bool] state
      that toggles back and forth between [true] and [false] whenever the
      [unit effect] is scheduled. *)

  module Toggle : sig
    type nonrec t = {
      state : bool t;
      set_state : (bool -> unit effect) t;
      toggle : unit effect t;
    }
    (** For the more advanced toggle function [Bonesai.toggle'] we return the
        state, the toggling function, and a function to set the state directly.
    *)
  end

  val toggle' : default_model:bool -> graph -> Toggle.t
  (** Just like [toggle] except that you also have an [effect] for directly
      setting the state in addition to toggling it back and forth. *)

  module Apply_action_context : sig
    (** A value with the type [('action, 'response) Apply_action_context.t] is
        provided to all state-machine's [apply_action] functions. It can be used
        to do a variety of things that are only legal inside of [apply_action]:

        1. Access the application time source directly. This is most likely
        useful to read the current time or sleep for some time span (TODO)

        2. "inject" a value corresponding to the state-machine's action type
        into an effect that can be scheduled.

        3. Directly schedule effects. *)

    type ('action, 'response) t

    val inject : ('action, 'response) t -> 'action -> 'response effect
    val schedule_event : _ t -> unit effect -> unit

    (* TODO *)
    (* val time_source : _ t -> Time_source.t *)
  end

  val state_machine :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    apply_action:
      (('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model) ->
    graph ->
    'model t * ('action -> unit effect) t
  (** [Bonesai.state_machine] allows you to build a state machine whose state is
      initialized to whatever you pass to [default_model], and the state machine
      transitions states using the [apply_action] function. The current state
      and a function for injecting an action into a schedulable effect are
      returned.

      [?equal] does the same thing that it does for [Bonesai.state], go read
      those docs for more. *)

  module IntMap : Map.S with type key = int
  module StringMap : Map.S with type key = string

  val assoc_int :
    ('k t -> 'v t -> graph -> 'r t) -> 'v IntMap.t t -> graph -> 'r IntMap.t t
  (** [assoc_*] are used to build a new instance of a Bonesai component for each
      element of a map.

      This function is very similar to [Map.S.mapi] and for good reason! It is
      doing the same thing (taking a map and a function and returning a new map
      with the function applied to every key-value pair), but this function does
      it with the Bonesai values, which means that the computation is done
      incrementally and also maintains a state machine for every key-value pair.
  *)

  val assoc_string :
    ('k t -> 'v t -> graph -> 'r t) ->
    'v StringMap.t t ->
    graph ->
    'r StringMap.t t
end
