open Tyxml

module WeakTable = Weak_ptr.Registry

type 'a handler = 'a -> unit Bonesai.effect

type subscription =
  | OnClick of unit handler
  | OnInput of string handler

type app_context =
  { recurse: bool
  ; mutable update: id:string -> html:string -> unit (* TODO use effects instead? *)
  ; subscriptions: subscription WeakTable.t
  }

let dummy_update ~id ~html =
  Dream.log "%s: unhandled update: %s" id html

type html_context = {
  recurse: bool;
  component_id: string;
  global_subscriptions: subscription WeakTable.t;
  mutable local_subscriptions: subscription list;
}

type 'a component = {
  deep :  'a Html.elt (* recursively rendered document *);
  shallow :  'a Html.elt (* non-recursive rendering *);
  local_subscriptions: subscription list;
} (* component has two HTML representations and a list of subscriptions.
The latter is required to prevent gc of subscriptions; we only access them
via the weak table. *)

module Html = struct
  include Html

  let js_side_event_handler (ctx: html_context) subscription =
    let id = WeakTable.register ctx.global_subscriptions subscription in
    let () = ctx.local_subscriptions <- subscription :: ctx.local_subscriptions in
    let name =
      match subscription with
      | OnClick _ -> "onclick"
      | OnInput _ -> "oninput"
    in
    Printf.sprintf "liveview_%s('%s', event)" name (WeakTable.string_of_id id)

  let a_onclick ctx handler =
    let sub = OnClick handler in
    a_onclick (js_side_event_handler ctx sub)

  let a_oninput (ctx: html_context) handler =
    let sub = OnInput handler in
    a_oninput (js_side_event_handler ctx sub)

  let sub_component (ctx : html_context) c =
    if ctx.recurse then c.deep else c.shallow
end

module Component = struct
  let div render (ctx: app_context) =
    let id = "42TODO" (* TODO how get id in bonesai? *) in
    let () = Dream.log "%s: render" id in
    let elts, local_subscriptions =
      let ctx : html_context =
        { component_id = id
        ; local_subscriptions = []
        ; global_subscriptions = ctx.subscriptions
        ; recurse = ctx.recurse
        }
      in
      let elts = render ctx in
      elts, ctx.local_subscriptions
    in
    let deep =
      (* TODO There should be a way to render only when the recursive view is actually used *)
      Html.(div ~a:[a_id id] elts)
    in
    let shallow = Html.(div ~a:[a_id id; a_user_data "morphdom-skip" ""] []) in
    if not ctx.recurse then
      (* TODO we currently send redundant updates: the entire branch from the component back to the root. Avoid! *)
      ctx.update ~id ~html:(Format.asprintf "%a" (Html.pp_elt ()) deep);
    { deep
    ; shallow
    ; local_subscriptions }
end

type 'a app = app_context -> Bonesai.graph -> 'a component Bonesai.t

module Dream = struct
  let prerender app =
    let ctx = { recurse = true; update = dummy_update; subscriptions = WeakTable.create () } in
    let app = Bonesai.Runtime.compile (app ctx) in
    let rendered_html =  Bonesai.Runtime.observe app in
    rendered_html.deep

  module Message = struct
    open Ppx_yojson_conv_lib.Yojson_conv.Primitives

    type event_from_client =
      | OnClick
      | OnInput of string
      [@@deriving yojson]

    type from_client =
      | Event of string * event_from_client
      | Info of string
      [@@deriving yojson]

    let from_client str =
      (* TODO catch errors *)
      let json = Yojson.Safe.from_string str in
      Ok (from_client_of_yojson json)

    type to_client =
      | Updates of (string * string) list (* component id, html *)
      [@@deriving yojson]

    let to_client x =
      let json = yojson_of_to_client x in
      Yojson.Safe.to_string json

  end

  let lookup id table =
    match WeakTable.id_of_string id with
    | None -> Error `Invalid_id
    | Some id ->
        match WeakTable.find_opt table id with
        | None -> Error `Not_found
        | Some x -> Ok x

  let effect_of_event_and_sub event sub =
    match event, sub with
    | Message.OnClick, OnClick f -> Some (f ())
    | OnInput s, OnInput f -> Some (f s)
    | _ -> None

  let apply app_context app effect =
    let updates = ref [] in
    let update ~id ~html =
      updates := (id, html) :: !updates
    in
    app_context.update <- update;
    Bonesai.Runtime.schedule_effect app effect;
    (* TODO; do we have to wait for synchronization? Like `flush` ind JS Bonsai? *)
    app_context.update <- dummy_update;
    !updates

  let run app websocket =
    let ctx = { recurse = false; update = dummy_update; subscriptions = WeakTable.create () } in
    let app = Bonesai.Runtime.compile (app ctx) in
    let%lwt () = Dream.send websocket "hello socket" in
    let rec loop () =
      match%lwt Dream.receive websocket with
      | None -> Lwt.return ()
      | Some msg ->
          match Message.from_client msg with
           (* Note, we here use ../weak_ptr and rely on the GC to
            * purge stuff that's not relevant anymore. High latency might still
            * induce situations where the GC removes a handler that some incoming
            * messages assume to be present. So some sort of user feedback is
            * required on the client side.
            *)
          | Ok (Event (subid, event)) ->
            begin
              let msg = "received event: " ^ msg in
              let%lwt () = Dream.send websocket msg in
              begin match lookup subid ctx.subscriptions with
              | Ok sub ->
                begin match effect_of_event_and_sub event sub with
                | None ->
                  let msg = "error: event/subscription mismatch: " ^ msg in
                  let%lwt () = Dream.send websocket msg in
                  loop ()
                | Some effect ->
                  let updates =
                    Dream.log "%s: activate" subid;
                    apply ctx app effect
                  in
                  let msg = Message.to_client (Updates updates) in
                  let%lwt () = Dream.send websocket msg in
                  loop ()
                end
              | Error `Invalid_id ->
                let msg = "error: invalid subscription id: " ^ subid in
                let%lwt () = Dream.send websocket msg in
                loop ()
              | Error `Not_found ->
                let msg = "error: subscription id not found: " ^ subid in
                let%lwt () = Dream.send websocket msg in
                loop ()
              end
            end
          | Ok (Info info) ->
            let msg = "received info: " ^ info in
            let%lwt () = Dream.send websocket msg in
            loop ()
          | Error emsg ->
            let msg = "error: cannot parse message: \""
                      ^ msg ^ "\": " ^ emsg
            in
            let%lwt () = Dream.send websocket msg in
            loop ()
    in loop ()
end
