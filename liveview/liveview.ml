open Tyxml

type 'a inject = 'a -> unit Bonesai.Expert.Effect.t

type packed_inject =
  | Unit : unit inject -> packed_inject
  | String : string inject -> packed_inject

(* pack functions, exposed publicly as 'a handled_type *)
let unit x = Unit x
let string x = String x

type 'a handled_type = 'a inject -> packed_inject

module WeakTable = Ephemeron.K1.Make (String)

(* We have two things that live both on the client and on the server:
   components and event handlers. We identify them using strings.
   Janestreet Bonsai has sophisticated tooling to generate stable identifiers,
   so called paths, for all values. I'm lazy and just use serial number
   converted to hex strings. *)
type app_context = {
  handlers : packed_inject WeakTable.t; (* handler id -> handler *)
  mutable next_component_id : int;
  mutable next_handler_id : int;
}
(* My intention with the WeakTable was to garbage collect redundant handlers
   after they stop being relevant. This was introduced while handlers where
   still allocated at render time. Now, with the static (at graph build time)
   handlers (and ids), this seems to be less relevant. But it may become
   relevant again, when I start working on dynamic lists and assoc/maps. *)

let app_context () =
  { handlers = WeakTable.create 7; next_component_id = 0; next_handler_id = 0 }

module Extra = struct
  type t = app_context

  let create = app_context
end

module Bonesai = Bonesai.Expert.Make (Bonesai.Expert.Effect) (Extra)

type graph = Bonesai.graph

let extra g : app_context = Bonesai.extra g

type 'a value = 'a Bonesai.t

(* allocate a new effect handler id *)
let handler_id (g : graph) =
  (* graph argument is to enforce that this is not called at runtime *)
  let ctx = extra g in
  let id = Printf.sprintf "e%x" ctx.next_handler_id in
  ctx.next_handler_id <- ctx.next_handler_id + 1;
  Bonesai.return id

(* allocate a new component id *)
let component_id (g : graph) =
  (* graph argument is to enforce that this is not called at runtime *)
  let ctx = extra g in
  let id = Printf.sprintf "c%x" ctx.next_component_id in
  ctx.next_component_id <- ctx.next_component_id + 1;
  Bonesai.return id

module Handler : sig
  type 'a t

  val id : 'a t -> string
  val create : id:string -> 'a handled_type -> 'a inject -> graph -> 'a t
end = struct
  (* this module enforces that handlers are correctly registered in the
     app_context *)

  type 'a t = { id : string; inject : 'a inject [@warning "-unused-field"] }
  (* The inject is not actually used, it's there to avoid garbage collection,
     handlers are accessed through the ctx.handlers weak hash table. *)

  let id t = t.id

  let create ~id pack inject graph =
    let ctx = extra graph in
    let () = WeakTable.add ctx.handlers id (pack inject) in
    { id; inject }
end

type 'a handler = 'a Handler.t

let handler' inject (pack : _ handled_type) to_action graph =
  let open Bonesai.Let_syntax in
  let+ inject = inject and+ id = handler_id graph in
  let f arg = inject (to_action arg) in
  Handler.create ~id pack f graph

let handler inject action = handler' inject unit (fun () -> action)

module Render : sig
  (** algebraic effect magic to eliminate context argument in the Html API *)

  type mode =
    | Pre
        (** Preview, components are rendered recursively, handlers and
            identifies for interactivity are omitted *)
    | Full  (** Components are rendered recursively, with interactivity *)
    | Update  (** With interactivity, but sub-components are omitted *)

  val with_mode : mode -> ('a -> 'b) -> 'a -> 'b

  val get_mode : unit -> mode
  (** this performs an effect and must be wrapped in [with_mode] *)

  val mode_to_string : mode -> string
end = struct
  (* hide effect and effect handler *)

  type mode = Pre | Full | Update
  type _ Effect.t += Get_mode : unit -> mode Effect.t

  let get_mode () = Effect.perform (Get_mode ())

  let with_mode (mode : mode) f x =
    let open Effect in
    let open Effect.Deep in
    try_with f x
      {
        effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Get_mode () ->
                Some (fun (k : (a, _) continuation) -> continue k mode)
            | _ -> None);
      }

  let mode_to_string = function
    | Full -> "Full"
    | Update -> "Update"
    | Pre -> "Pre"
end

type 'a renderer = Render.mode -> 'a Html.elt
type packed_renderer = Renderer : 'a renderer -> packed_renderer
type 'a component = { render : 'a renderer; hole : 'a Html.elt }

module Html = struct
  include Html

  let js_event_handler attr name h =
    match Render.get_mode () with
    | Pre -> a_user_data "liveview" name
    | Full | Update ->
        let js =
          Printf.sprintf "liveview_%s('%s', event)" name (Handler.id h)
        in
        attr js

  let a_onclick (h : unit handler) = js_event_handler a_onclick "onclick" h
  let a_oninput (h : string handler) = js_event_handler a_oninput "oninput" h

  let sub_component (c : _ component) =
    match Render.get_mode () with
    | Update -> c.hole
    | Full -> c.render Full
    | Pre -> c.render Pre
end

type _ Effect.t +=
  | Update : { id : string; r : packed_renderer } -> unit Effect.t

(* TODO: add Update module here that puts some guardrails around the Update
   effect, like Render does around the mode effect. *)

module Component = struct
  open Bonesai.Let_syntax

  type ('outer, 'inner) container = {
    full : id:string -> Render.mode -> 'inner Html.elt list -> 'outer Html.elt;
    hole : id:string -> 'outer Html.elt;
  }

  let div =
    let open Html in
    let full ~id = function
      | Render.Pre -> div ~a:[ a_user_data "liveview" "component" ]
      | Full | Update -> div ~a:[ a_id id ]
    and hole ~id = div ~a:[ a_id id; a_user_data "morph-skip" "" ] [] in
    { full; hole }

  let t container id render =
    let render mode =
      let () = Dream.log "%s: render %s" id (Render.mode_to_string mode) in
      let elts = Render.with_mode mode render () in
      container.full ~id mode elts
    and hole = container.hole ~id in
    Effect.perform (Update { id; r = Renderer render });
    { render; hole }

  let cutoff (c : 'a component value) : 'a component value =
    (* the current type of components does not have anything of use for
       parent components. So they should never fire an update.

       TODO this is not true actually: subcomponents access the render in Full
       mode. But this only called once, during initialization of the app,
       before any updates happen. This could (and should) be refactored such
       that Full mode can actually only be called before any updates happen. *)
    Bonesai.cutoff ~equal:(fun _ _ -> true) c

  let arg1 container a render graph =
    let raw =
      let+ a = a and+ id = component_id graph in
      t container id (fun () -> render a)
    in
    cutoff raw

  let arg2 container a b render graph =
    let raw =
      let+ a = a and+ b = b and+ id = component_id graph in
      t container id (fun () -> render a b)
    in
    cutoff raw

  let arg3 container a b c render graph =
    let raw =
      let+ a = a and+ b = b and+ c = c and+ id = component_id graph in
      t container id (fun () -> render a b c)
    in
    cutoff raw

  let arg4 container a b c d render graph =
    let raw =
      let+ a = a
      and+ b = b
      and+ c = c
      and+ d = d
      and+ id = component_id graph in
      t container id (fun () -> render a b c d)
    in
    cutoff raw
end

type 'a app = graph -> ([< Html_types.flow5 ] as 'a) component value

let with_update_handler (update : id:string -> packed_renderer -> unit) f x =
  let open Effect in
  let open Effect.Deep in
  try_with f x
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Update { id; r } ->
              Some (fun (k : (a, _) continuation) -> continue k (update ~id r))
          | _ -> None);
    }

let without_update_handler f = with_update_handler (fun ~id:_ _ -> ()) f

let prerender app =
  (* TODO this is currently building the entire bonesai graph just to evaluate
     it once and then leave it to the gc. Can we avoid building the graph for
     the prerender? *)
  let f () =
    let app, _ctx = Bonesai.Runtime.compile app in
    let obs = Bonesai.Runtime.observe app in
    obs.render Pre
  in
  without_update_handler f ()

module Dream_websocket = struct
  module Message = struct
    open Ppx_yojson_conv_lib.Yojson_conv.Primitives

    type event_from_client = OnClick | OnInput of string [@@deriving yojson]

    type from_client = Event of string * event_from_client | Info of string
    [@@deriving yojson]

    let from_client str =
      try
        let json = Yojson.Safe.from_string str in
        Ok (from_client_of_yojson json)
      with exn -> Error (Printexc.to_string exn)

    type to_client =
      | Updates of (string * string) list (* component id, html *)
      | Info of string
      | Error of string
    [@@deriving yojson]

    let to_client x =
      let json = yojson_of_to_client x in
      Yojson.Safe.to_string json

    let send ws m =
      let json = to_client m in
      Dream.send ws json

    let send_info ws m = send ws (Info m)
    let send_updates ws x = send ws (Updates x)
    let send_error ws m = send ws (Error m)
  end

  let lookup id table =
    match WeakTable.find_opt table id with
    | None -> Error `Not_found
    | Some x -> Ok x

  let effect_of_event_and_handler event handler =
    match (event, handler) with
    | Message.OnClick, Unit f -> Some (f ())
    | OnInput s, String f -> Some (f s)
    | _ -> None

  module Updates : sig
    type t

    val create : unit -> t * (id:string -> packed_renderer -> unit)
    val send : t -> Dream.websocket -> unit Lwt.t
  end = struct
    type e = { mutable html : string; mutable cnt : int }
    type t = (string, e) Hashtbl.t

    let create () =
      let ht = Hashtbl.create 7 in
      let fn ~id (Renderer f) =
        let dom = f Update in
        let html = Format.asprintf "%a" (Html.pp_elt ()) dom in
        match Hashtbl.find_opt ht id with
        | Some e ->
            e.cnt <- e.cnt + 1;
            e.html <- html
        | None -> Hashtbl.add ht id { cnt = 1; html }
      in
      (ht, fn)

    let send t socket =
      let lst = Hashtbl.fold (fun id e acc -> (id, e.html) :: acc) t [] in
      let%lwt () = Message.send_updates socket lst in
      (* debugging output to client *)
      Lwt_stream.iter_s
        (fun (id, e) ->
          (* TODO think hard, whether redundant renders are a programming error *)
          if e.cnt > 1 then
            Message.send_info socket
              (Printf.sprintf "caught %i redundant updates of component %s"
                 e.cnt id)
          else Lwt.return_unit)
        (Hashtbl.to_seq t |> Lwt_stream.of_seq)
  end

  let apply app effect =
    let updates, handler = Updates.create () in
    let f () =
      Bonesai.Runtime.schedule_effect app effect;
      (* TODO; do we have to wait for synchronization? Like `flush` in JS Bonsai? *)
      updates
    in
    with_update_handler handler f ()

  let run app websocket =
    let app, ctx = without_update_handler Bonesai.Runtime.compile app in
    let%lwt () =
      (* initialization: update entire DOM recursively. Sets component ids and js event handlers *)
      let obs = Bonesai.Runtime.observe app in
      let html =
        let open Html in
        div ~a:[ a_id "liveview" ] [ obs.render Full ]
      in
      let str = Format.asprintf "%a" (Html.pp_elt ()) html in
      Message.send_updates websocket [ ("liveview", str) ]
    in
    let rec loop () =
      let () =
        Gc.full_major ()
        (* make sure that the WeakTable stuff works *)
        (* TODO remove before release *)
      in
      match%lwt Dream.receive websocket with
      | None -> Lwt.return ()
      | Some msg -> (
          match Message.from_client msg with
          | Ok (Event (id, event)) -> begin
              begin match lookup id ctx.handlers with
              | Ok sub -> begin
                  match effect_of_event_and_handler event sub with
                  | None ->
                      let msg = "event/handler mismatch: " ^ msg in
                      let%lwt () = Message.send_error websocket msg in
                      loop ()
                  | Some effect ->
                      let%lwt () =
                        Lwt_unix.sleep 0.5
                        (* add latency to test tolerance *)
                        (* TODO remove before release *)
                      in
                      let updates =
                        Dream.log "%s: apply" id;
                        apply app effect
                      in
                      let%lwt () = Updates.send updates websocket in
                      loop ()
                end
              | Error `Invalid_id ->
                  let msg = "error: invalid event id: " ^ id in
                  let%lwt () = Message.send_error websocket msg in
                  loop ()
              | Error `Not_found ->
                  (* Does high latency induce situations where the GC removes a
                   * handler that some incoming messages assume to be present?
                   *
                   * I think this cannot happen currently. Handlers are
                   * allocated statically at graph build time. Their ids do not
                   * change.
                   *
                   * This might change when implementing dynamic
                   * lists/assoc/maps. In that case, components might be
                   * deallocated while the client is still sending events.
                   * E.g. user deletes list element, then edits an input in
                   * that element, before receiving the update that actually
                   * deletes the element. But in that case it is sound to
                   * ignore the delayed event (this match case) on the server.
                   *
                   * Now, if this can happen contrary to my expectation, we'll
                   * need to add some user feedback about unstable connection
                   * on the client.
                   *
                   * TODO upgrade to bug / server side error?
                   *)
                  let msg = "error: no event handler found for id: " ^ id in
                  let%lwt () = Message.send_error websocket msg in
                  loop ()
              end
            end
          | Ok (Info _info) -> loop ()
          | Error emsg ->
              let msg = "cannot parse message: \"" ^ msg ^ "\": " ^ emsg in
              let%lwt () = Message.send_error websocket msg in
              loop ())
    in
    loop ()

  let dream_tyxml ~csrf_token x =
    let js = Printf.sprintf "liveview_boot('%s')" csrf_token in
    let html =
      let open Html in
      html
        (head
           (title (txt "Liveview"))
           [
             script ~a:[ a_src "/liveview.js" ] (txt "");
             script ~a:[ a_src "/idiomorph.js" ] (txt "");
             script (cdata_script js);
           ])
        (body [ div ~a:[ a_id "liveview" ] [ x ] ])
    in
    let str = Format.asprintf "%a" (Html.pp ()) html in
    Dream.html str

  let get_main app req =
    match Dream.header req "Upgrade" with
    | Some "websocket" -> begin
        match Dream.query req "csrf_token" with
        | None -> Dream.empty `Unauthorized
        | Some token -> (
            match%lwt Dream.verify_csrf_token req token with
            | `Expired _ -> Dream.empty `Forbidden
            | `Wrong_session -> Dream.empty `Forbidden
            | `Invalid -> Dream.empty `Unauthorized
            | `Ok ->
                let app = app req in
                Dream.websocket (run app))
      end
    | _ ->
        let csrf_token = Dream.csrf_token req in
        let app = app req in
        let bonesai_html = prerender app in
        dream_tyxml ~csrf_token bonesai_html

  let get_crunch type_ fname _req =
    (* TODO maybe; we can get fancy here and use Crunch.hash to support etag
     * based hashing *)
    match Crunch.read fname with
    | None -> Dream.empty (`Status 500)
    | Some content -> Dream.respond ~headers:[ ("Content-Type", type_) ] content

  let handler app =
    let open Dream in
    router
      [
        get "/" (get_main app);
        get "/favicon.ico" (get_crunch "image/x-icon" "favicon.ico");
        get "/idiomorph.js" (get_crunch "text/javascript" "idiomorph.js");
        get "/liveview.js" (get_crunch "text/javascript" "liveview.js");
      ]
end

let dream = Dream_websocket.handler
