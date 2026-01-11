type app_context
type html_context
type 'a handler

val handler :
  ('action -> unit Bonesai.effect) Bonesai.t ->
  'action ->
  app_context ->
  Bonesai.graph ->
  unit handler Bonesai.t

val handler' :
  ('action -> unit Bonesai.effect) Bonesai.t ->
  ('arg -> 'action) ->
  app_context ->
  Bonesai.graph ->
  'arg handler Bonesai.t

type 'a component

module Html : sig
  (** use like Tyxml.Html *)

  include module type of Tyxml.Html

  val a_onclick :
    html_context -> unit handler -> [> `OnClick | `User_data ] attrib

  val a_oninput :
    html_context -> string handler -> [> `OnInput | `User_data ] attrib

  val sub_component : html_context -> 'a component -> 'a elt
end

type component_id

val component_id : app_context -> Bonesai.graph -> component_id Bonesai.t

module Component : sig
  (* wrappers around Html.{div,...} that create new liveview components *)

  open Bonesai
  open Html_types

  type ('outer, 'inner) container

  val div : ([> div ], [< div_content_fun ]) container

  val arg1 :
    ('outer, 'inner) container ->
    'a t ->
    ('a -> html_context -> 'inner Html.elt list) ->
    app_context ->
    graph ->
    'outer component t

  val arg2 :
    ('outer, 'inner) container ->
    'a t ->
    'b t ->
    ('a -> 'b -> html_context -> 'inner Html.elt list) ->
    app_context ->
    graph ->
    'outer component t

  val arg3 :
    ('outer, 'inner) container ->
    'a t ->
    'b t ->
    'c t ->
    ('a -> 'b -> 'c -> html_context -> 'inner Html.elt list) ->
    app_context ->
    graph ->
    'outer component t

  val arg4 :
    ('outer, 'inner) container ->
    'a t ->
    'b t ->
    'c t ->
    'd t ->
    ('a -> 'b -> 'c -> 'd -> html_context -> 'inner Html.elt list) ->
    app_context ->
    graph ->
    'outer component t
end

type 'a app =
  app_context ->
  Bonesai.graph ->
  ([< Html_types.flow5 ] as 'a) component Bonesai.t

module Dream : sig
  val prerender : 'a app -> 'a Html.elt
  val run : 'a app -> Dream.websocket -> unit Lwt.t

  val handler : (Dream.request -> 'a app) -> Dream.handler
  (** Dream request handler for running the app over websockets *)
end
