open Tyxml
module WeakTable = Ephemeron.K1.Make (String)

type 'a inject = 'a -> unit Bonesai.effect
type 'a value = 'a Bonesai.t

type packed_inject =
  | Unit : unit inject -> packed_inject
  | String : string inject -> packed_inject

type app_context = {
  handlers : packed_inject WeakTable.t;
  mutable next_component_id : int;
  mutable next_handler_id : int;
}

type graph = app_context * Bonesai.graph

let to_bonesai (_, graph) = graph

type 'a renderer = shallow:bool -> pure:bool -> 'a Html.elt
type packed_renderer = Renderer : 'a renderer -> packed_renderer
type render_mode = Pre | Full | Update

type _ Effect.t +=
  | Update : { id : string; r : packed_renderer } -> unit Effect.t
  | Get_render_mode : unit -> render_mode Effect.t

type 'a handler = {
  id : string;
  f : 'a inject; [@warning "-unused-field"] (* exists for gc only, see below *)
}

type 'a handled_type = 'a inject -> packed_inject

let handler_id ((ctx, _) : graph) =
  (* graph argument is to enforce that this is not called at runtime *)
  let id = Printf.sprintf "e%x" ctx.next_handler_id in
  ctx.next_handler_id <- ctx.next_handler_id + 1;
  Bonesai.return id

let handler' inject (pack : _ handled_type) to_action (ctx, graph) =
  let open Bonesai.Let_syntax in
  let+ inject = inject and+ id = handler_id (ctx, graph) in
  let f arg = inject (to_action arg) in
  let () = WeakTable.add ctx.handlers id (pack f) in
  (* My intention with the WeakTable was to garbage collect redundant handlers
     after they stop being relevant. Now with the static (at graph build time)
     handler (and ids), this seems to be less relevant. Maybe it becomes
     relevant again, when I start working on list with dynamic number of
     elements or assoc/maps.

     However, to avoid gc of the handler, we here return it as part of the
     identified handler type, although it won't be used anywhere downstream.
     Just the type of the handler is relevant, to ensure that server side
     handlers match client side handlers.
  *)
  { id; f }

let unit handler = Unit handler
let string handler = String handler
let handler inject action = handler' inject unit (fun () -> action)

type 'a component = { render : 'a renderer; hole : pure:bool -> 'a Html.elt }

module Html = struct
  include Html

  let js_event_handler attr name h =
    match Effect.perform (Get_render_mode ()) with
    | Pre -> a_user_data "liveview" name
    | Full | Update ->
        let js = Printf.sprintf "liveview_%s('%s', event)" name h.id in
        attr js

  let a_onclick (h : unit handler) = js_event_handler a_onclick "onclick" h
  let a_oninput (h : string handler) = js_event_handler a_oninput "oninput" h

  let sub_component (c : _ component) =
    match Effect.perform (Get_render_mode ()) with
    | Update -> c.hole ~pure:false
    | Full -> c.render ~shallow:false ~pure:false
    | Pre -> c.render ~shallow:false ~pure:true
end

let component_id ((ctx, _) : graph) =
  (* graph argument is to enforce that this is not called at runtime *)
  let id = Printf.sprintf "c%x" ctx.next_component_id in
  ctx.next_component_id <- ctx.next_component_id + 1;
  Bonesai.return id

module Component = struct
  open Bonesai.Let_syntax

  type ('outer, 'inner) container = {
    full : id:string -> pure:bool -> 'inner Html.elt list -> 'outer Html.elt;
    hole : id:string -> pure:bool -> 'outer Html.elt;
  }
  (* TODO pure hole does not make much sense; it's three modes not four *)

  let div =
    let open Html in
    let id_attr ~pure id =
      if pure then Html.a_user_data "liveview" "component" else Html.a_id id
    in
    let full ~id ~pure elts = div ~a:[ id_attr ~pure id ] elts
    and hole ~id ~pure =
      div ~a:[ id_attr ~pure id; a_user_data "morph-skip" "" ] []
    in
    { full; hole }

  let t container id render =
    let render ~shallow ~pure =
      let () = Dream.log "%s: render ~shallow:%b ~pure:%b" id shallow pure in
      let elts = render () in
      container.full ~id ~pure elts
    and hole ~pure = container.hole ~id ~pure in
    (* TODO Simplify renderer and component types. I think
       - the renderer in the update is always called with ~pure:false ~shallow:true
       - hole is never ~pure
       - hole can be reconstructed from the container type and id
       - render is never called with ~shallow
       - wait ... sub_component does ... so I have to think more

       ... I think the render type should be render_mode -> html this function
       should handle the Get_render_mode effect locally.
    *)
    Effect.perform (Update { id; r = Renderer render });
    { render; hole }

  let cutoff (c : 'a component value) : 'a component value =
    (* the current type of components does not have anything of use for
       parent components. So they should never fire an update. *)
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

let app_context () =
  { handlers = WeakTable.create 7; next_component_id = 0; next_handler_id = 0 }

let with_render_mode (mode : render_mode) f x =
  let open Effect in
  let open Effect.Deep in
  try_with f x
    {
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Get_render_mode () ->
              Some (fun (k : (a, _) continuation) -> continue k mode)
          | _ -> None);
    }

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
  let ctx = app_context () in
  let bonesai graph = app (ctx, graph) in
  let f () =
    let app = Bonesai.Runtime.compile bonesai in
    let obs = Bonesai.Runtime.observe app in
    obs.render ~pure:true ~shallow:false
  in
  without_update_handler (with_render_mode Pre f) ()

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
        let dom =
          let f () = f ~shallow:true ~pure:false in
          with_render_mode Update f ()
        in
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
    let ctx = app_context () in
    let bonesai graph = app (ctx, graph) in
    let app = without_update_handler Bonesai.Runtime.compile bonesai in
    let%lwt () =
      (* initialization: update entire DOM recursively. Sets component ids and js event handlers *)
      let obs = Bonesai.Runtime.observe app in
      (* TODO the internal API for rendering / building stuff could be simpler,
         shallow pure seems redundant here. *)
      let rendered =
        with_render_mode Full
          (fun () -> obs.render ~shallow:false ~pure:false)
          ()
      in
      let html =
        let open Html in
        div ~a:[ a_id "liveview" ] [ rendered ]
      in
      let str = Format.asprintf "%a" (Html.pp_elt ()) html in
      Message.send_updates websocket [ ("liveview", str) ]
    in
    let rec loop () =
      let () =
        Gc.full_major ()
        (* make sure that the WeakTable stuff works *)
      in
      match%lwt Dream.receive websocket with
      | None -> Lwt.return ()
      | Some msg -> (
          match Message.from_client msg with
          (* Note, we here use ../weak_ptr and rely on the GC to
           * purge stuff that's not relevant anymore. High latency might still
           * induce situations where the GC removes a handler that some incoming
           * messages assume to be present. So some sort of user feedback is
           * required on the client side.
           *)
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
