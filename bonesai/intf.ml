module type Types = sig
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

  (* TODO rename t -> value ? *)

  type 'a effect

  type graph
  (** [Bonesai.graph] is a required parameter to all Bonesai functions which do
      more than pure computation. The value is always [local_] because Bonesai
      applications have two phases:

      1. The graph building phase. This is the phase where you have access to a
      [local_ Bonesai.graph]

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

  val create : start:'a raw -> graph -> 'a data * ('a action -> unit effect) t
  (** Instantiate an incremental container with given start value *)

  val value : 'a data -> 'a raw t
  (** Bonesai value representing the current contents of the container *)

  (* TODO bring in remaining stuff from ReactiveData.S, incl. fold & map *)
end

module type Bonesai = sig
  (** Adapted from Janestreet's Bonsai library, late 2025 / early 2026.
      https://github.com/janestreet/bonsai/blob/8e6c34dceff46c92c3db58d7801da805417139ad/src/cont.mli
  *)

  include Types

  type 'a t
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

  val state_machine_with_input :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    apply_action:
      (('action, unit) Apply_action_context.t ->
      'input ->
      'model ->
      'action ->
      'model) ->
    'input t ->
    graph ->
    'model t * ('action -> unit effect) t
  (** [Bonesai.state_machine_with_input] is identical to [Bonesai.state_machine]
      except that you can pass an arbitrary ['a Bonesai.t] dependency and have
      access to the current value within the [apply_action] function. *)

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
         and type 'a effect := 'a effect
         and type 'a t := 'a t

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
         and type 'a effect := 'a effect
         and type 'a t := 'a t

    val filter : (M.key -> 'a -> bool) -> 'a data -> 'a data
  end
end
