module Bonesai : Bonesai.T

type graph = Bonesai.graph

type 'a value = 'a Bonesai.value

type 'a to_task = 'a -> unit Bonesai.task

type 'a event_handler

(* TODO add some mechanism to handle dynamic lists/assocs/maps *)

val event_handler :
  'action to_task value -> 'action -> graph -> unit event_handler value

val string_event_handler :
     'action to_task value
  -> (string -> 'action)
  -> graph
  -> string event_handler value

(* Note, one might wonder whether the event handler functions could be as follows:

   val event_handler: unit task value -> graph -> unit event_handler value
   val string_event_handler: (string -> unit task) value -> graph -> string event_handler value

   Yes they could, but this causes overhead for the users. What currently works like this:

   let state, to_task =
     Bonesai.state_machine graph ~default_model:start ~apply_action
   in
   let incr = event_handler to_task Incr graph in

   would become

   let state, to_task =
     Bonesai.state_machine graph ~default_model:start ~apply_action
   in
   let incr = event_handler (Bonesai.map ~f:(fun to_task -> to_task Incr) to_task) graph in
*)

type 'a component

module Html : sig
  (** Extension of {!Tyxml.Html} that allows rendering sub-components and
      injecting liveview events on the client. *)

  (** @closed *)
  include module type of Tyxml.Html

  val sub_component : 'a component -> 'a elt

  val a_onclick : unit event_handler -> [> `OnClick | `User_data] attrib

  val a_oninput : string event_handler -> [> `OnInput | `User_data] attrib
end

module Component : sig
  (** Wrappers around {!Html.div} to create new liveview components. *)

  type ('outer, 'inner) container

  val div : ([> Html_types.div], [< Html_types.div_content_fun]) container

  val arg1 :
       ('outer, 'inner) container
    -> 'a value
    -> ('a -> 'inner Html.elt list)
    -> graph
    -> 'outer component value

  val arg2 :
       ('outer, 'inner) container
    -> 'a value
    -> 'b value
    -> ('a -> 'b -> 'inner Html.elt list)
    -> graph
    -> 'outer component value

  val arg3 :
       ('outer, 'inner) container
    -> 'a value
    -> 'b value
    -> 'c value
    -> ('a -> 'b -> 'c -> 'inner Html.elt list)
    -> graph
    -> 'outer component value

  val arg4 :
       ('outer, 'inner) container
    -> 'a value
    -> 'b value
    -> 'c value
    -> 'd value
    -> ('a -> 'b -> 'c -> 'd -> 'inner Html.elt list)
    -> graph
    -> 'outer component value
end

(*
module LList : sig
  (** This solves a common pattern: rendering a component with a dynamic set of
      sub-components. *)

  open Bonesai
  open Component

  type ('a, 'el) template = graph -> ('a value * 'el component value)

  type invalid_index_result = (unit, [`Invalid_index]) result

  type ('a, 'el, 'ret) action =
    | Insert: int * ('a, 'el) template -> ('a, 'el, invalid_index_result) action
    | Append: ('a, 'el) template -> ('a, 'el, unit) action
    | Swap: int * int -> ('a, 'el, invalid_index_result) action
    | Remove: int -> ('a, 'el, invalid_index_result) action

  val insert: int -> ('a, 'el) template -> ('a, 'el, invalid_index_result) action
  val insert_ignore_error: int -> ('a, 'el) template -> ('a, 'el, unit) action
  val append: ('a, 'el) template -> ('a, 'el, unit) action
  val swap_ignore_error: int * int -> ('a, 'el, unit) action
  val remove: int -> ('a, 'el, invalid_index_result) action
  val remove_ignore_error: int -> ('a, 'el, unit) action

  val create :
       ('outer, 'el) container
    -> start:(('a, 'el) template) list
    -> graph
    -> 'a list value
       * (('a, 'el, 'res) action -> 'res task) value
       * 'outer component value
end
*)

module LMap (Ord : Map.OrderedType) : sig
  (** Extends {!Stdlib.Map.Make} with {!Bonesai} and {!Component} magic. This
      solves a common pattern: rendering a component with a dynamic set of
      sub-components.

      TODO extend API for ordering.
      TODO think again whether the key is actually needed. React apparently
      needs it for the reconciliation algorithm. Things might be different in
      my liveview setting. In particular, the child components already have
      unique identifiers. If we get rid of the key, we can just use a list or
      similar. The ordering then happens automatically. *)

  (** @closed *)
  include Map.S with type key = Ord.t

  open Bonesai
  open Component

  type ('a, 'el) template = graph -> 'a value * 'el component value

  type ('a, 'el) action = Set of key * ('a, 'el) template | Del of key
  (* TODO use GADT like above in LList to inform users about invalid key errors *)

  val create :
       ('outer, 'el) container
    -> start:('a, 'el) template t
    -> graph
    -> 'a t value
       * (('a, 'el) action -> unit task) value
       * 'outer component value
end

type 'a app = graph -> ([< Html_types.flow5] as 'a) component value

val prerender : 'a app -> 'a Html.elt

type template =
     Html_types.head_content_fun Html.elt list
  -> Html_types.div Html.elt
  -> Html_types.html Html.elt

val dream :
  ?slowdown:float -> template -> (Dream.request -> 'a app) -> Dream.handler
(** Dream request handler for running the app over websockets. [~slowdown:s]
    delays server-side event handling by [s] seconds. *)
