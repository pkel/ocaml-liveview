type app_context
type html_context
type handler_id

val handler_id : app_context -> Bonesai.graph -> handler_id Bonesai.t

type 'a component

module Html : sig
  (** use like Tyxml.Html *)

  type 'a handler = handler_id * ('a -> unit Bonesai.effect)

  include module type of Tyxml.Html

  val a_onclick : html_context -> unit handler -> [> `OnClick ] attrib
  val a_oninput : html_context -> string handler -> [> `OnInput ] attrib
  val sub_component : html_context -> 'a component -> 'a elt
end

type component_id

val component_id : app_context -> Bonesai.graph -> component_id Bonesai.t

module Component : sig
  (* wrappers around Html.{div,...} that create new liveview components *)
  open Html_types

  val div :
    component_id ->
    (html_context -> [< div_content_fun ] Html.elt list) ->
    app_context ->
    [> `Div ] component
end

type 'a app = app_context -> Bonesai.graph -> 'a component Bonesai.t

module Dream : sig
  val prerender : 'a app -> 'a Html.elt
  val run : 'a app -> Dream.websocket -> unit Lwt.t

  val handler : (Dream.request -> [ `Div ] app) -> Dream.handler
  (** Dream request handler for running the app over websockets *)
end
