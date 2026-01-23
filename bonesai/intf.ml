module type Types = sig
  type 'a value
  (** The primary type in this library, you can think of ['a value] as "an ['a]
      that changes over time". The two main ways to create ['a value] are:

      1. by creating a state machine
      - the current state of the state machine is returned as a ['state value]
      - the "send the state machine an action" function is also inside of a
        ['a value]

      2. by mapping on existing [value]'s to derive a new computed [value]

      Janestreet's Bonsai calls this ['a Bonsai.t]. *)

  type 'a task
  (** An ['a task] represents a delayed computation that returns an ['a]. Such
      tasks might be scheduled arbitrarily often (including zero times) in the
      future. They might trigger side-effects and not idempotent. There is no
      memoization. It might be helpful to think of ['a task = unit -> 'a] but we
      intentionally hide the representation to avoid programmers creating tasks
      manually. Janestreet's Bonsai calls this ['a effect]. *)

  type graph
  (** [graph] is a required parameter to all Bonesai functions which do more
      than pure computation.

      1. The graph building phase. This is the phase where you have access to a
      [graph].

      2. Runtime. The application has started and modifying the graph is no
      longer permitted.

      Note: It seems Janestreet's Bonsai enforces that the graph is local to the
      function, using recent features of OxCaml. On base OCaml we have to rely
      on users to not store a reference to the graph at build time to use it
      later at runtime. *)
end

module type Data = sig
  (** Shared signature for incremental data structures; adapted from
      {!ReactiveData.S}. *)

  (* ReactiveData.S has a note saying "most functions in this interface
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

  val create : start:'a raw -> graph -> 'a data * ('a action -> unit task) value
  (** Instantiate an incremental container with given start value. *)

  val value : 'a data -> 'a raw value
  (** Bonesai value representing the current contents of the container. *)

  (* TODO bring in remaining stuff from ReactiveData.S, incl. fold & map *)
end

module type Bonesai = sig
  (** Janestreet's Bonsai, reimplemented without Janestreet libraries, using the
      OCaml Stdlib, {!React}, and {!ReactiveData} instead.

      The API is adapted from Janestreets Bonsai.Cont API from late 2025 / early
      2026. See
      https://github.com/janestreet/bonsai/blob/8e6c34dceff46c92c3db58d7801da805417139ad/src/cont.mli
  *)

  (* TODO add some high level description what we're doing. Like in
     Janestreet's Bonsai. Build a graph. Nodes represent stateful computations.
     Edges represent dependencies between the computations. Runtime passes
     values between computations and ensures everything is up to date, avoiding
     redundant evaluations whenever computation inputs are constant. *)

  include Types

  val return : 'a -> 'a value
  (** [return] produces a [value] whose inner value is constant. *)

  val map : 'a value -> f:('a -> 'b) -> 'b value
  (** [map], [map2], and [both] are ways to build a new [value] which is
      dependent on the values of other [value]s. *)

  val map2 : 'a value -> 'b value -> f:('a -> 'b -> 'c) -> 'c value
  val both : 'a value -> 'b value -> ('a * 'b) value

  module Let_syntax : sig
    val ( let+ ) : 'a value -> ('a -> 'b) -> 'b value
    val ( and+ ) : 'a value -> 'b value -> ('a * 'b) value
  end

  val cutoff : 'a value -> equal:('a -> 'a -> bool) -> 'a value
  (** The runtime will recompute a node if any of its dependencies change. But
      sometimes, you may want to consider two contained values to be "close
      enough" and skip re-computation of the dependent values. You can do that
      by passing a custom equality function to [cutoff]. *)

  val state :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model value * ('model -> unit task) value
  (** [state] allocates a stateful value. It returns both the [value] containing
      the current state, as well as a [value] containing a function for
      overwriting the state.

      You must provide a "starting" value for the state.

      [?equal] (default [phys_equal]) can be used to integrate a {!cutoff}. *)

  val state_opt :
    ?equal:('model -> 'model -> bool) ->
    ?default_model:'model ->
    graph ->
    'model option value * ('model option -> unit task) value
  (** [state_opt] is just like [state] except that the model is optional. The
      model starts out as [None] unless you provide a value to the
      [default_model] optional parameter. *)

  val state' :
    ?equal:('model -> 'model -> bool) ->
    'model ->
    graph ->
    'model value * (('model -> 'model) -> unit task) value
  (** Similar to [state], but the `set` function takes a function that
      calculates the new state from the previous state. *)

  val toggle : default_model:bool -> graph -> bool value * unit task value
  (** [toggle] is a small helper function for building a [bool] state that
      toggles back and forth between [true] and [false] whenever the [unit task]
      is scheduled. *)

  module Toggle : sig
    type t = {
      state : bool value;
      set_state : (bool -> unit task) value;
      toggle : unit task value;
    }
    (** For the more advanced toggle function [toggle'] we return the state, the
        toggling function, and a function to set the state directly. *)
  end

  val toggle' : default_model:bool -> graph -> Toggle.t
  (** Just like [toggle] except that you also have an [task] for directly
      setting the state in addition to toggling it back and forth. *)

  module Apply_action_context : sig
    (** A value with the type [('action, 'response) Apply_action_context.t] is
        provided to all state-machine's [apply_action] functions. It can be used
        to do a variety of things that are only legal inside of [apply_action]:

        1. Turn the state-machine's action type into a task that can be
        scheduled.

        2. Directly schedule tasks for execution. *)

    type ('action, 'response) t

    val to_task : ('action, 'response) t -> 'action -> 'response task
    val schedule : _ t -> unit task -> unit

    (* TODO Janestreet's Bonsai exposes a time source here.

        3. Access the application time source directly. This is most likely
        useful to read the current time or sleep for some time span

       val time_source : _ t -> Time_source.t *)
  end

  val state_machine :
    ?equal:('model -> 'model -> bool) ->
    default_model:'model ->
    apply_action:
      (('action, unit) Apply_action_context.t -> 'model -> 'action -> 'model) ->
    graph ->
    'model value * ('action -> unit task) value
  (** [state_machine] allows you to build a state machine whose state is
      initialized to whatever you pass to [default_model], and the state machine
      transitions states using the [apply_action] function. The current state
      and a function for turning an action into a task are returned.

      [?equal] works like the argument to {!state}. *)

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
    'model value * ('action -> unit task) value
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
    'model value * ('action -> 'return task) value
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
    'model value * ('action -> 'return task) value
  (** [actor_with_input] is just like [actor] but it can witness the current
      value of a [value] inside its [recv] function just like
      [state_machine_with_input] *)

  (* TODO Janestreet's Bonsai has another actor that allows to specify the
     return type per action using GADTs. *)

  module BList : sig
    (** Incremental list data structure; wrapper around {!ReactiveData.RList}.
    *)

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
         and type 'a task := 'a task
         and type 'a value := 'a value

    (* TODO much stuff of RList is missing *)
  end

  module BMap (M : Map.S) : sig
    (** Incremental Map data structure; wrapper around {!ReactiveData.RMap}. *)

    type 'a patch = [ `Add of M.key * 'a | `Del of M.key ] list

    include
      Data
        with type 'a patch := 'a patch
         and type 'a raw := 'a M.t
         and type graph := graph
         and type 'a task := 'a task
         and type 'a value := 'a value

    val filter : (M.key -> 'a -> bool) -> 'a data -> 'a data
  end
end
