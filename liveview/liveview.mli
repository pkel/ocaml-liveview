module Bonesai : Bonesai.T

type graph = Bonesai.graph
type 'a value = 'a Bonesai.value
type 'a to_task = 'a -> unit Bonesai.task
type 'a event_handler

(* TODO add some mechanism to handle dynamic lists/assocs/maps *)

val event_handler :
  'action to_task value -> 'action -> graph -> unit event_handler value

val string_event_handler :
  'action to_task value ->
  (string -> 'action) ->
  graph ->
  string event_handler value

type 'a component

module Html : sig
  (** Extension of {!Tyxml.Html} that allows rendering sub-components and
      injecting liveview events on the client. *)

  include module type of Tyxml.Html
  (** @closed *)

  val sub_component : 'a component -> 'a elt
  val a_onclick : unit event_handler -> [> `OnClick | `User_data ] attrib
  val a_oninput : string event_handler -> [> `OnInput | `User_data ] attrib
end

module Component : sig
  (** Wrappers around {!Html.div} to create new liveview components. *)

  type ('outer, 'inner) container

  val div : ([> Html_types.div ], [< Html_types.div_content_fun ]) container

  val arg1 :
    ('outer, 'inner) container ->
    'a value ->
    ('a -> 'inner Html.elt list) ->
    graph ->
    'outer component value

  val arg2 :
    ('outer, 'inner) container ->
    'a value ->
    'b value ->
    ('a -> 'b -> 'inner Html.elt list) ->
    graph ->
    'outer component value

  val arg3 :
    ('outer, 'inner) container ->
    'a value ->
    'b value ->
    'c value ->
    ('a -> 'b -> 'c -> 'inner Html.elt list) ->
    graph ->
    'outer component value

  val arg4 :
    ('outer, 'inner) container ->
    'a value ->
    'b value ->
    'c value ->
    'd value ->
    ('a -> 'b -> 'c -> 'd -> 'inner Html.elt list) ->
    graph ->
    'outer component value
end

type 'a app = graph -> ([< Html_types.flow5 ] as 'a) component value

val prerender : 'a app -> 'a Html.elt

val dream : (Dream.request -> 'a app) -> Dream.handler
(** Dream request handler for running the app over websockets. *)
