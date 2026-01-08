type app_context
type html_context
type 'a handler = 'a -> unit Bonesai.effect
type 'a component

module Html : sig
  (** use like Tyxml.Html *)

  include module type of Tyxml.Html

  val a_onclick : html_context -> unit handler ->  [> `OnClick ] attrib
  val a_oninput : html_context -> string handler ->  [> `OnInput ] attrib

  val sub_component : html_context -> 'a component -> 'a elt
end

module Component : sig
  (* wrappers around Html.{div,...} that create new liveview components *)

  open Html_types

  val div: (html_context -> [< div_content_fun ] Html.elt list)  -> app_context -> [> `Div ] component
end

type 'a app = app_context -> Bonesai.graph -> 'a component Bonesai.t

module Dream : sig
  val prerender: 'a app -> 'a Html.elt
  val run: 'a app -> Dream.websocket -> unit Lwt.t
end
