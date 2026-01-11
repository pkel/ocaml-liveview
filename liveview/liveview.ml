open Tyxml
module WeakTable = Ephemeron.K1.Make (String)

type 'a handler0 = 'a -> unit Bonesai.effect
type subscription = OnClick of unit handler0 | OnInput of string handler0
type 'a renderer = shallow:bool -> pure:bool -> 'a Html.elt
type packed_renderer = Renderer : 'a renderer -> packed_renderer

type app_context = {
  mutable update : (id:string -> packed_renderer -> unit) option;
      (* TODO use effects instead? *)
  subscriptions : subscription WeakTable.t;
  mutable next_component_id : int;
  mutable next_handler_id : int;
}

type html_context = {
  shallow : bool;
  pure : bool;
  global_subscriptions : subscription WeakTable.t;
}

type 'a component = { render : 'a renderer; hole : pure:bool -> 'a Html.elt }
type component_id = string
type handler_id = string

let component_id (ctx : app_context) _graph =
  (* graph argument is to enforce that this is not called at runtime *)
  let id = Printf.sprintf "c%x" ctx.next_component_id in
  ctx.next_component_id <- ctx.next_component_id + 1;
  Bonesai.return id

let handler_id (ctx : app_context) _graph =
  (* graph argument is to enforce that this is not called at runtime *)
  let id = Printf.sprintf "e%x" ctx.next_handler_id in
  ctx.next_handler_id <- ctx.next_handler_id + 1;
  Bonesai.return id

module Html = struct
  include Html

  type 'a handler = handler_id * ('a -> unit Bonesai.effect)

  let js_side_event_handler (ctx : html_context) id subscription =
    let () = WeakTable.add ctx.global_subscriptions id subscription in
    let name =
      match subscription with OnClick _ -> "onclick" | OnInput _ -> "oninput"
    in
    Printf.sprintf "liveview_%s('%s', event)" name (*WeakTable.string_of_id*) id

  let a_onclick ctx (id, handler) =
    if ctx.pure then a_user_data "liveview" "onclick"
    else
      let sub = OnClick handler in
      a_onclick (js_side_event_handler ctx id sub)

  let a_oninput (ctx : html_context) (id, handler) =
    if ctx.pure then a_user_data "liveview" "oninput"
    else
      let sub = OnInput handler in
      a_oninput (js_side_event_handler ctx id sub)

  let sub_component (ctx : html_context) c =
    if ctx.shallow then c.hole ~pure:ctx.pure
    else c.render ~pure:ctx.pure ~shallow:ctx.shallow
end

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

  let t container id render (ctx : app_context) =
    let render ~shallow ~pure =
      let () = Dream.log "%s: render ~shallow:%b ~pure:%b" id shallow pure in
      let elts =
        let ctx : html_context =
          { global_subscriptions = ctx.subscriptions; shallow; pure }
        in
        render ctx
      in
      container.full ~id ~pure elts
    and hole ~pure = container.hole ~id ~pure in
    let () =
      match ctx.update with Some upd -> upd ~id (Renderer render) | None -> ()
    in
    { render; hole }

  open Bonesai

  let cutoff (c : 'a component t) : 'a component t =
    (* the current type of components does not have anything of use for
       parent components. So they should never fire an update. *)
    cutoff ~equal:(fun _ _ -> true) c

  let arg1 container a render ctx graph =
    let raw =
      let+ a = a and+ id = component_id ctx graph in
      t container id (render a) ctx
    in
    cutoff raw

  let arg2 container a b render ctx graph =
    let raw =
      let+ a = a and+ b = b and+ id = component_id ctx graph in
      t container id (render a b) ctx
    in
    cutoff raw

  let arg3 container a b c render ctx graph =
    let raw =
      let+ a = a and+ b = b and+ c = c and+ id = component_id ctx graph in
      t container id (render a b c) ctx
    in
    cutoff raw

  let arg4 container a b c d render ctx graph =
    let raw =
      let+ a = a
      and+ b = b
      and+ c = c
      and+ d = d
      and+ id = component_id ctx graph in
      t container id (render a b c d) ctx
    in
    cutoff raw
end

type 'a app =
  app_context ->
  Bonesai.graph ->
  ([< Html_types.flow5 ] as 'a) component Bonesai.t

module Dream = struct
  let app_context () =
    {
      update = None;
      subscriptions = WeakTable.create 7;
      next_component_id = 0;
      next_handler_id = 0;
    }

  let prerender app =
    let ctx = app_context () in
    let app = Bonesai.Runtime.compile (app ctx) in
    let obs = Bonesai.Runtime.observe app in
    obs.render ~pure:true ~shallow:false

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

  let effect_of_event_and_sub event sub =
    match (event, sub) with
    | Message.OnClick, OnClick f -> Some (f ())
    | OnInput s, OnInput f -> Some (f s)
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
        let dom = f ~shallow:true ~pure:false in
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

  let apply app_context app effect =
    let updates, update = Updates.create () in
    app_context.update <- Some update;
    Bonesai.Runtime.schedule_effect app effect;
    (* TODO; do we have to wait for synchronization? Like `flush` in JS Bonsai? *)
    app_context.update <- None;
    updates

  let run app websocket =
    let ctx = app_context () in
    let app = Bonesai.Runtime.compile (app ctx) in
    let%lwt () =
      (* initialization: update entire DOM recursively. Sets component ids and js event handlers *)
      let obs = Bonesai.Runtime.observe app in
      let html =
        let open Html in
        div ~a:[ a_id "liveview" ] [ obs.render ~shallow:false ~pure:false ]
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
          | Ok (Event (subid, event)) -> begin
              begin match lookup subid ctx.subscriptions with
              | Ok sub -> begin
                  match effect_of_event_and_sub event sub with
                  | None ->
                      let msg = "event/subscription mismatch: " ^ msg in
                      let%lwt () = Message.send_error websocket msg in
                      loop ()
                  | Some effect ->
                      let%lwt () =
                        Lwt_unix.sleep 0.5
                        (* add latency to test tolerance *)
                      in
                      let updates =
                        Dream.log "%s: activate" subid;
                        apply ctx app effect
                      in
                      let%lwt () = Updates.send updates websocket in
                      loop ()
                end
              | Error `Invalid_id ->
                  let msg = "error: invalid subscription id: " ^ subid in
                  let%lwt () = Message.send_error websocket msg in
                  loop ()
              | Error `Not_found ->
                  let msg = "error: subscription id not found: " ^ subid in
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
