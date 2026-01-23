module type Task = sig
  (** Task are opaque boxes of future computations. Consumers of the Bonesai API
      never create or execute tasks manually. Experts can instantiate Bonesai
      with their own task type. *)

  type 'a t

  val create : (unit -> 'a) -> 'a t
  val execute : 'a t -> 'a
end

module Task : Task = struct
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
    val schedule : 'a app -> unit task -> unit
    val observe : 'a app -> 'a
  end
end

module NoExtra = struct
  type t = unit

  let create () = ()
end

module Make (Task : Task) (Extra : Extra) :
  WithExtra with type extra = Extra.t and type 'a task = 'a Task.t = struct
  type 'a task = 'a Task.t
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
    let schedule _app = Task.execute
  end

  let state ?(equal = phys_equal) start _graph =
    let signal, setter = React.S.create ~eq:equal start in
    let set_task new_model = Task.create (fun () -> setter new_model) in
    (signal, React.S.const set_task)

  let state_opt ?(equal = phys_equal) ?default_model _graph =
    let equal a b =
      match (a, b) with Some a, Some b -> equal a b | _ -> false
    in
    state ~equal default_model _graph

  let state' ?(equal = phys_equal) start _graph =
    let signal, setter = React.S.create ~eq:equal start in
    let set_task update =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          setter (update old_model))
    in
    (signal, React.S.const set_task)

  let toggle ~default_model _graph =
    let signal, setter = React.S.create ~eq:phys_equal default_model in
    let toggle_task =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          setter (not old_model))
    in
    (signal, React.S.const toggle_task)

  module Toggle = struct
    type t = {
      state : bool value;
      set_state : (bool -> unit task) value;
      toggle : unit task value;
    }
  end

  let toggle' ~default_model _graph =
    let signal, setter = React.S.create ~eq:phys_equal default_model in
    let set_task new_model = Task.create (fun () -> setter new_model) in
    let toggle_task =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          setter (not old_model))
    in
    Toggle.
      {
        state = signal;
        set_state = React.S.const set_task;
        toggle = React.S.const toggle_task;
      }

  module Apply_action_context = struct
    type ('action, 'response) t = {
      to_task : 'action -> 'response task;
      schedule : unit task -> unit;
    }

    let to_task t = t.to_task
    let schedule t = t.schedule
  end

  let state_machine ?(equal = phys_equal) ~default_model ~apply_action _graph =
    let open Apply_action_context in
    let signal, setter = React.S.create ~eq:equal default_model in
    let rec ctx = { to_task; schedule = Task.execute }
    and to_task action =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          let new_model = apply_action ctx old_model action in
          setter new_model)
    in
    (signal, React.S.const to_task)

  let actor ?(equal = phys_equal) ~default_model ~recv _graph =
    let open Apply_action_context in
    let signal, setter = React.S.create ~eq:equal default_model in
    let rec ctx = { to_task; schedule = Task.execute }
    and to_task action =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          let new_model, return = recv ctx old_model action in
          let () = setter new_model in
          return)
    in
    (signal, React.S.const to_task)

  let state_machine_with_input ?(equal = phys_equal) ~default_model
      ~apply_action input _graph =
    let open Apply_action_context in
    let signal, setter = React.S.create ~eq:equal default_model in
    let rec ctx = { to_task; schedule = Task.execute }
    and to_task action =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          let input = React.S.value input in
          let new_model = apply_action ctx input old_model action in
          setter new_model)
    in
    (signal, React.S.const to_task)

  let actor_with_input ?(equal = phys_equal) ~default_model ~recv input _graph =
    let open Apply_action_context in
    let signal, setter = React.S.create ~eq:equal default_model in
    let rec ctx = { to_task; schedule = Task.execute }
    and to_task action =
      Task.create (fun () ->
          let old_model = React.S.value signal in
          let input = React.S.value input in
          let new_model, return = recv ctx input old_model action in
          let () = setter new_model in
          return)
    in
    (signal, React.S.const to_task)

  module BData (RData : ReactiveData.S) = struct
    type 'a patch = 'a RData.patch
    type 'a raw = 'a RData.data
    type 'a action = Patch of 'a patch | Set of 'a raw
    type 'a data = 'a RData.t

    let create ~start (_ : graph) =
      let data, handle = RData.create start in
      let to_task = function
        | Patch p -> Task.create (fun () -> RData.patch handle p)
        | Set d -> Task.create (fun () -> RData.set handle d)
      in
      (data, React.S.const to_task)

    let value data = RData.signal data
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
