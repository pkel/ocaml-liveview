module type Types = sig
  type 'a value
  (** The primary type in this library, you can think of ['a value] as "an ['a]
      that changes over time". The two main ways to create ['a value] are:

      1. by creating a state machine
      - the current state of the state machine is returned as a ['state value]
      - the "send the state machine an action" function is also inside of a
        ['a value]

      2. by mapping on existing [value]'s to derive a new computed [value] *)

  type 'a effect_

  (* TODO effect is a really bad name, as OCaml has its own effects now and
     they are something else. [effect] is a keyword, even. Call it delayed
     instead? *)

  type graph
  (** [graph] is a required parameter to all Bonesai functions which do more
      than pure computation. The value is always [local_] because Bonesai
      applications have two phases:

      (* TODO don't have _local, that's an OxCaml thing. Remove any references
      to it *)

      1. The graph building phase. This is the phase where you have access to a
      [local_ graph]

      2. Runtime. The application has started and modifying the graph is no
      longer permitted. *)
end

module type Data = sig
  (** shared signature for incremental data structures; adapted from
      ReactiveData.S *)

  (* ReactiveData.S has a note saying "most function in this interface
     are not safe to call during a React update step". I do not know what a
     React update step is. TODO learn about React update steps, make sure I
     don't break things. *)

  include Types

  type 'a data
  (** Incremental version of the data container *)

  type 'a raw
  (** Non-incremental version of the data container *)

  type 'a patch
  (** Patch format *)

  (** Action format *)
  type 'a action =
    | Patch of 'a patch
        (** [Patch p] triggers the application of p on the current contents *)
    | Set of 'a raw
        (** [Set d] replaces the entire container non-incrementally *)

  val create :
    start:'a raw -> graph -> 'a data * ('a action -> unit effect_) value
  (** Instantiate an incremental container with given start value *)

  val value : 'a data -> 'a raw value
  (** Bonesai value representing the current contents of the container *)

  (* TODO bring in remaining stuff from ReactiveData.S, incl. fold & map *)
end

module type Bonesai = sig
  (** Adapted from Janestreet's Bonsai library, late 2025 / early 2026.
      https://github.com/janestreet/bonsai/blob/8e6c34dceff46c92c3db58d7801da805417139ad/src/cont.mli
  *)

  include Types

  val return : 'a -> 'a value
  (** [return] produces a [value] whose inner value is constant. *)

  val map : 'a value -> f:('a -> 'b) -> 'b value
  (** [map], [map2], and [both] are ways to build a new [value] which is
      dependent on the values of other [value]. As noted above, you should
      prefer to use [let%arr] than these functions, because they come with some
      performance benefits. *)

  (* TODO don't have let%arr. Remove any references to it *)

  val map2 : 'a value -> 'b value -> f:('a -> 'b -> 'c) -> 'c value
  val both : 'a value -> 'b value -> ('a * 'b) value

  module Let_syntax : sig
    val ( let+ ) : 'a value -> ('a -> 'b) -> 'b value
    val ( and+ ) : 'a value -> 'b value -> ('a * 'b) value
  end

  val cutoff : 'a value -> equal:('a -> 'a -> bool) -> 'a value
  (** The runtime will recompute a node if any of its dependencies change. But
      sometimes, you may want to consider two contained values to be "close
      enough" and cut off recomputation. You can do that by passing a custom
      equality function to [cutoff]. *)

  val state :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model value * ('model -> unit effect_) value
  (** [state] allocates a stateful value. It returns both the [value] containing
      the current state, as well as a [value] containing a function for
      overwriting the state.

      You must provide a "starting" value for the state.

      [?equal] (default [phys_equal]) is used by some combinators to reduce
      memory usage. (E.g. [assoc] and [match%sub] may determine that they can
      store a reference to only the default model rather than the current
      model.) *)

  (* TODO the reference to assoc and match%sub seems relevant. Can/should I do
     the same? *)

  val state_opt :
    ?equal:('model -> 'model -> bool) ->
    ?default_model:'model ->
    graph ->
    'model option value * ('model option -> unit effect_) value
  (** [state_opt] is just like [state] except that the model is optional. The
      model starts out as [None] unless you provide a value to the
      [default_model] optional parameter. *)

  val state' :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model value * (('model -> 'model) -> unit effect_) value
  (** Similar to [state], but the `set` function takes a function that
      calculates the new state from the previous state. *)

  val toggle : default_model:bool -> graph -> bool value * unit effect_ value
  (** [toggle] is a small helper function for building a [bool] state that
      toggles back and forth between [true] and [false] whenever the
      [unit effect_] is scheduled. *)

  module Toggle : sig
    type t = {
      state : bool value;
      set_state : (bool -> unit effect_) value;
      toggle : unit effect_ value;
    }
    (** For the more advanced toggle function [toggle'] we return the state, the
        toggling function, and a function to set the state directly. *)
  end

  val toggle' : default_model:bool -> graph -> Toggle.t
  (** Just like [toggle] except that you also have an [effect_] for directly
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

    val inject : ('action, 'response) t -> 'action -> 'response effect_
    val schedule_event : _ t -> unit effect_ -> unit

    (* TODO *)
    (* val time_source : _ t -> Time_source.t *)
  end

  val state_machine :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    apply_action:
      (('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model) ->
    graph ->
    'model value * ('action -> unit effect_) value
  (** [state_machine] allows you to build a state machine whose state is
      initialized to whatever you pass to [default_model], and the state machine
      transitions states using the [apply_action] function. The current state
      and a function for injecting an action into a schedulable effect are
      returned.

      [?equal] does the same thing that it does for [state], go read those docs
      for more. *)

  val state_machine_with_input :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    apply_action:
      (('action, unit) Apply_action_context.t ->
      'input ->
      'model ->
      'action ->
      'model) ->
    'input value ->
    graph ->
    'model value * ('action -> unit effect_) value
  (** [state_machine_with_input] is identical to [state_machine] except that you
      can pass an arbitrary ['a value] dependency and have access to the current
      value within the [apply_action] function. *)

  val actor :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    recv:
      (('action, 'return) Apply_action_context.t ->
      'model ->
      'action ->
      'model * 'return) ->
    graph ->
    'model value * ('action -> 'return effect_) value
  (** [actor] is similar to [state_machine], but its [recv] function is
      responsible for not only transitioning the state of the state machine, but
      also for responding with a "return value" to whoever sent the message to
      the actor. *)

  val actor_with_input :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    recv:
      (('action, 'return) Apply_action_context.t ->
      'input ->
      'model ->
      'action ->
      'model * 'return) ->
    'input value ->
    graph ->
    'model value * ('action -> 'return effect_) value
  (** [actor_with_input] is just like [actor] but it can witness the current
      value of a [value] inside its [recv] function just like
      [state_machine_with_input] *)

  (* TODO Janestreet Bonsai has another actor that allows to specify the
     return type per action using GADTs. *)

  module BList : sig
    (** Incremental list data structure; wrapper around ReactiveData.RList *)

    (** Patch operation on lists. All operations are of linear complexity *)
    type 'a p =
      | I of int * 'a  (** [I (i, v)] adds [v] at position [i] *)
      | R of int  (** [R i] removes the [i]-th elements *)
      | U of int * 'a  (** [U (i, v)] substitutes the [i]-th element with [v] *)
      | X of int * int  (** [X (i, j)] swaps the [i]-th and [j]-th elements *)

    type 'a patch = 'a p list
    (** A patch is a list of operations. The operations are applied in the order
        they appear in the list.

        The indices correspond to list contents after the operations that appear
        earlier in the list have been applied, not to the contents before the
        whole patch operation.

        A patch comprised of I, R, and U steps with increasing indices can be
        applied in O(m+n), where m is the patch length and n is the current size
        of the list. Arbitrary patches are slower, requiring O(m*n). *)

    include
      Data
        with type 'a patch := 'a p list
         and type 'a raw := 'a list
         and type graph := graph
         and type 'a effect_ := 'a effect_
         and type 'a value := 'a value

    (* TODO much stuff of RList is missing *)
  end

  module BMap (M : Map.S) : sig
    (** Incremental Map data structure; wrapper around ReactiveData.Rmap *)

    type 'a patch = [ `Add of M.key * 'a | `Del of M.key ] list

    include
      Data
        with type 'a patch := 'a patch
         and type 'a raw := 'a M.t
         and type graph := graph
         and type 'a effect_ := 'a effect_
         and type 'a value := 'a value

    val filter : (M.key -> 'a -> bool) -> 'a data -> 'a data
  end
end
