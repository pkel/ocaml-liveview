module Bonesai : Bonesai.T

type graph = Bonesai.graph
type 'a value = 'a Bonesai.t
type 'a inject = 'a -> unit Bonesai.effect
type 'a handler

(* TODO add some mechanism to handle dynamic lists/assocs/maps *)

val handler : 'action inject value -> 'action -> graph -> unit handler value

type 'a handled_type

val string : string handled_type

val handler' :
  'action inject value ->
  'arg handled_type ->
  ('arg -> 'action) ->
  graph ->
  'arg handler value

type 'a component

module Html : sig
  (** use like Tyxml.Html *)

  include module type of Tyxml.Html

  val a_onclick : unit handler -> [> `OnClick | `User_data ] attrib
  val a_oninput : string handler -> [> `OnInput | `User_data ] attrib
  val sub_component : 'a component -> 'a elt
end

module Component : sig
  (* wrappers around Html.{div,...} that create new liveview components *)
  type ('outer, 'inner) container

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

  val div : ([> Html_types.div ], [< Html_types.div_content_fun ]) container
end

type 'a app = graph -> ([< Html_types.flow5 ] as 'a) component value

val prerender : 'a app -> 'a Html.elt

val dream : (Dream.request -> 'a app) -> Dream.handler
(** Dream request handler for running the app over websockets. *)
