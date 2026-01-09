module Html = Tyxml.Html
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

module LiveView : sig
  type 'a handler = 'a -> unit Bonsai.Effect.t

  type subscription =
    (* TODO hide implementation *)
    | OnClick of unit handler
    | OnInput of string handler

  type app_context
  type html_context
  type 'a component

  val component :
    app_context ->
    string ->
    (html_context -> [< Html_types.div_content_fun ] Html.elt list) ->
    [> `Div ] component
  (* TODO first argument is component's id (Bonsai.path_id). Hide this technicality here *)

  module Html : sig
    include module type of Html

    val a_onclick : html_context -> unit handler -> [> `OnClick ] attrib
    val a_oninput : html_context -> string handler -> [> `OnInput ] attrib
    val sub_component : html_context -> 'a component -> 'a Html.elt
  end

  val initial_html :
    (app_context -> Bonsai.graph -> 'a component Bonsai.t) -> 'a Html.elt

  type 'a app

  val app : (app_context -> Bonsai.graph -> 'a component Bonsai.t) -> 'a app

  val apply :
    'a app ->
    unit Bonsai.Effect.t ->
    (string * string) list (* TODO clarify that this is component id and html *)

  val subscriptions : 'a app -> (string * subscription) list (* TODO remove *)
end = struct
  type 'a handler = 'a -> unit Bonsai.Effect.t
  type subscription = OnClick of unit handler | OnInput of string handler

  type app_context = {
    recurse : bool;
    mutable update :
      id:string -> html:string -> unit (* TODO use effects instead? *);
  }

  let dummy_update ~id ~html = Dream.log "%s: unhandled update: %s" id html

  type html_context = {
    recurse : bool;
    component_id : string;
    mutable subscriptions : (string * subscription) list;
  }

  let add_subscription ctx id sub =
    ctx.subscriptions <- (id, sub) :: ctx.subscriptions

  type 'a component = {
    html : 'a Html.elt;
    hole : 'a Html.elt;
    subscriptions : (string * subscription) list;
  }

  module Html = struct
    include Html

    let js_side_event_handler id subscription =
      let name =
        match subscription with
        | OnClick _ -> "onclick"
        | OnInput _ -> "oninput"
      in
      Printf.sprintf "liveview_%s('%s', event)" name id

    let a_onclick (ctx : html_context) handler =
      let len = List.length ctx.subscriptions in
      let id = Printf.sprintf "%s:%i" ctx.component_id len in
      let sub = OnClick handler in
      add_subscription ctx id sub;
      a_onclick (js_side_event_handler id sub)

    let a_oninput (ctx : html_context) handler =
      let len = List.length ctx.subscriptions in
      let id = Printf.sprintf "%s:%i" ctx.component_id len in
      let sub = OnInput handler in
      add_subscription ctx id sub;
      a_oninput (js_side_event_handler id sub)

    let sub_component ctx c =
      List.iter (fun (id, sub) -> add_subscription ctx id sub) c.subscriptions;
      if ctx.recurse then c.html else c.hole
  end

  let component (ctx : app_context) id render =
    let () = Dream.log "%s: render" id in
    let elts, subscriptions =
      let ctx : html_context =
        { component_id = id; subscriptions = []; recurse = ctx.recurse }
      in
      let elts = render ctx in
      (elts, ctx.subscriptions)
    in
    let html = Html.(div ~a:[ a_id id ] elts) in
    let hole = Html.(div ~a:[ a_id id; a_user_data "morphdom-skip" "" ] []) in
    if not ctx.recurse then
      (* TODO we currently send redundant updates: the entire branch from the component back to the root. Avoid! *)
      ctx.update ~id ~html:(Format.asprintf "%a" (Html.pp_elt ()) html);
    { html; hole; subscriptions }

  type 'a app = app_context * 'a component Bonsai_driver.t

  let driver bonsai =
    let clock =
      let now = Core.Time_ns.now () in
      Bonsai.Time_source.create ~start:now
    in
    Bonsai_driver.create ~clock bonsai

  let html x =
    let r = Bonsai_driver.result x in
    r.html

  let initial_html bonsai =
    let context = { recurse = true; update = dummy_update } in
    let driver = driver (bonsai context) in
    html driver

  let app bonsai =
    let context = { recurse = false; update = dummy_update } in
    (context, driver (bonsai context))

  let apply (ctx, driver) effect =
    (* TODO can effects help clean up the accumulation of updates? *)
    let updates = ref [] in
    let update ~id ~html = updates := (id, html) :: !updates in
    let () =
      ctx.update <- update;
      let open Bonsai_driver in
      schedule_event driver effect;
      flush driver;
      trigger_lifecycles driver (* TODO maybe defer this *);
      ctx.update <- dummy_update
    in
    !updates

  let subscriptions (_, driver) =
    let r = Bonsai_driver.result driver in
    r.subscriptions
end

module Counter = struct
  open LiveView

  module Action = struct
    type t = Increment | Decrement [@@deriving sexp_of]
  end

  let component ~start ctx graph : [> `Div ] component Bonsai.t =
    let state, inject =
      Bonsai.state_machine0 graph ~sexp_of_model:[%sexp_of: Core.Int.t]
        ~equal:[%equal: Int.t] ~sexp_of_action:[%sexp_of: Action.t]
        ~default_model:start
        ~apply_action:(fun
            (_ : _ Bonsai.Apply_action_context.t) model ->
          function
        | Action.Increment -> model + 1
        | Action.Decrement -> model - 1)
    and id = Bonsai.path_id graph in
    let%arr state = state and inject = inject and id = id in
    let render ctx =
      let open Html in
      let button label_ action =
        let handler () = inject action in
        button ~a:[ a_onclick ctx handler ] [ txt label_ ]
      in
      [
        button "-1" Action.Decrement;
        txt (Int.to_string state);
        button "+1" Action.Increment;
      ]
    in
    component ctx id render
end

module Input = struct
  open LiveView

  let component ~start ctx graph : [> `Div ] component Bonsai.t =
    let state, inject =
      Bonsai.state_machine0 graph ~sexp_of_model:Core.Sexp.of_string
        ~equal:String.equal ~sexp_of_action:Core.Sexp.of_string
        ~default_model:start
        ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _old new_ ->
          new_)
    and id = Bonsai.path_id graph in
    let%arr state = state and inject = inject and id = id in
    let render ctx =
      let open Html in
      [
        form
          ~a:[ a_action "."; a_method `Post ]
          [
            input
              ~a:[ a_input_type `Text; a_oninput ctx inject; a_value state ]
              ();
          ];
      ]
    in
    component ctx id render
end

let main ~n1 ~n2 ~n3 ~s ctx graph =
  let open LiveView in
  let%sub one = Counter.component ~start:n1 ctx graph in
  let%sub two = Counter.component ~start:n2 ctx graph in
  let%sub three = Counter.component ~start:n3 ctx graph in
  let%sub four = Input.component ~start:s ctx graph in
  let%sub id = Bonsai.path_id graph in
  let%arr one = one
  and two = two
  and three = three
  and four = four
  and id = id in
  let open Html in
  let render ctx =
    [
      sub_component ctx one;
      sub_component ctx two;
      sub_component ctx three;
      sub_component ctx four;
    ]
  in
  component ctx id render

(* In some way or another, the initial state has to be persisted across
 * requests; from initial page load to websocket open. There are a couple of
 * options:
 * - client side: cookie or query parameters to the websocket request, maybe
 *   encrypted and/or signed.
 * - server side: session data in memory or some distributed store like redis
 * I guess this decision is best left to the user. Have to provide a suitable
 * API though.
 *
 * One somewhat sensible approach might be to put all query params from the
 * page load onto the websocket as well. This should get us pretty far.
 * Problems will arise if the page load is indeterministic, think randomness or
 * updates to the backend db by other clients between page load and web socket
 * open.
 * ... I do this for now on the JS/client side.
 *
 * begin state init logic
 *)

let parse_query_s req =
  match Dream.query req "s" with None -> "init" | Some s -> s

let parse_query_n1 req =
  match Dream.query req "n1" with
  | None -> 42
  | Some x -> ( match int_of_string_opt x with None -> 42 | Some x -> x)

let parse_query_n2 req =
  match Dream.query req "n2" with
  | None -> 13
  | Some x -> ( match int_of_string_opt x with None -> 13 | Some x -> x)

let parse_query_n3 req =
  match Dream.query req "n3" with
  | None -> 21
  | Some x -> ( match int_of_string_opt x with None -> 21 | Some x -> x)

let main req =
  let n1 = parse_query_n1 req
  and n2 = parse_query_n2 req
  and n3 = parse_query_n3 req
  and s = parse_query_s req in
  main ~n1 ~n2 ~n3 ~s

(* end state init logic *)

let dream_tyxml ~csrf_token x =
  let js =
    Printf.sprintf
      "\n\
      \    console.log('js: hello js');\n\
      \    const loc = window.location;\n\
      \    const tok = '%s';\n\
      \    var ws_url;\n\
      \    if (loc.search) {\n\
      \      ws_url = \
       `//${loc.host}${loc.pathname}${loc.search}&csrf_token=${tok}`;\n\
      \    } else {\n\
      \      ws_url = `//${loc.host}${loc.pathname}?csrf_token=${tok}`;\n\
      \    }\n\
      \    const socket = new WebSocket(ws_url);\n\n\
      \    function patch_component(id, html) {\n\
      \      const component = document.getElementById(id);\n\
      \      morphdom(component, html, {\n\
      \        childrenOnly: true,\n\
      \        onBeforeElUpdated: (fromEl, toEl) => {\n\
      \          if (toEl.hasAttribute('data-morphdom-skip')) {\n\
      \            return false; // skip updating this element\n\
      \          } else {\n\
      \            return true;\n\
      \          };\n\
      \        },\n\
      \      });\n\
      \    };\n\n\
      \    socket.onmessage = function (e) {\n\
      \      var obj = false;\n\
      \      try {\n\
      \        obj = JSON.parse(e.data);\n\
      \      } catch (err) {\n\
      \        console.log('ws rcv: (parse error)', e.data);\n\
      \        return;\n\
      \      }\n\
      \      console.log('ws rcv:', obj);\n\
      \      if (obj && obj.hasOwnProperty('updates')) {\n\
      \        Object.keys(obj.updates).forEach(component_id => {\n\
      \          patch_component(component_id, obj.updates[component_id]);\n\
      \        });\n\
      \      };\n\
      \    };\n\n\
      \    const queue = []\n\n\
      \    function send_message(m) {\n\
      \      console.log('ws snd: ' + m);\n\
      \      if (socket.readyState !== 1) {\n\
      \        queue.push(m);\n\
      \      } else {\n\
      \        socket.send(m);\n\
      \      }\n\
      \    };\n\n\
      \    socket.onopen = function () {\n\
      \      while (queue.length > 0) {\n\
      \        socket.send(queue.shift());\n\
      \      };\n\
      \    };\n\n\
      \    function liveview_handler(name, arg1) {\n\
      \      return ((id, event) => {\n\
      \        event.preventDefault();\n\
      \        event.stopPropagation();\n\n\
      \        var msg = name + '|' + id;\n\
      \        if (arg1) {\n\
      \          msg += '|' + arg1(event);\n\
      \        }\n\
      \        send_message(msg);\n\
      \      })\n\
      \    };\n\n\
      \    const liveview_onclick = liveview_handler('onclick')\n\
      \    const liveview_oninput = liveview_handler('oninput', e => \
       e.target.value)\n\n\
      \    console.log('js: ready js');\n\n\
      \    send_message('info|load 1');\n\
      \    send_message('info|load 2');\n\
      \    "
      csrf_token
  in
  let html =
    let open Html in
    html
      (head
         (title (txt "Counter"))
         [
           script (cdata_script js);
           script
             ~a:
               [
                 a_src
                   "https://cdn.jsdelivr.net/npm/morphdom@2.7.5/dist/morphdom-umd.min.js";
               ]
             (txt "");
         ])
      (body [ x ])
  in
  let str = Format.asprintf "%a" (Html.pp ()) html in
  Dream.html str

module Message = struct
  (* TODO use json as message format *)

  open Angstrom

  type event = OnClick | OnInput of string
  type t = Event of string * event | Info of string

  let id = take_while (fun c -> c <> '|')
  let onclick = string "onclick|" *> id >>| fun i -> Event (i, OnClick)

  let oninput =
    string "oninput|" *> id >>= fun i ->
    char '|' *> take_while (fun _ -> true) >>| fun s -> Event (i, OnInput s)

  let info = string "info|" *> take_while (fun _ -> true) >>| fun s -> Info s
  let t_parser = choice [ onclick; oninput; info ]
  let parse_t = parse_string ~consume:Consume.All t_parser
end

let effect_of_event_and_sub event sub =
  match (event, sub) with
  | Message.OnClick, LiveView.OnClick f -> Some (f ())
  | OnInput s, OnInput f -> Some (f s)
  | _ -> None

let liveview main websocket =
  let bonsai_app = LiveView.app main in
  let%lwt () = Dream.send websocket "hello socket" in
  let rec loop () =
    match%lwt Dream.receive websocket with
    | None -> Lwt.return ()
    | Some msg -> (
        match Message.parse_t msg with
        (* TODO Tracking last subscriptions might not be enough. Assume
         * client triggers second event before the server can handle the
         * first. This can lead to routing errors. One solution can be to
         * maintain a ring buffer of recent subscriptions. Each event would
         * include the lowest id still relevant; the event handler would
         * purge all subscriptions not relevant any more.
         *
         * Note, with the approach of ../weak_ptr we'd rely on the GC to
         * purge stuff that's not relevant anymore. High latency might still
         * induce situations where the GC removes a handler that some incoming
         * messages assume to be present. So some sort of user feedback is
         * required on the client side.
         *)
        | Ok (Event (subid, event)) -> begin
            let msg = "received event: " ^ msg in
            let%lwt () = Dream.send websocket msg in
            let bonsai_subscriptions = LiveView.subscriptions bonsai_app in
            begin match List.assoc_opt subid bonsai_subscriptions with
            | Some sub -> begin
                match effect_of_event_and_sub event sub with
                | None ->
                    let msg = "error: event/subscription mismatch: " ^ msg in
                    let%lwt () = Dream.send websocket msg in
                    loop ()
                | Some effect ->
                    let updates =
                      Dream.log "%s: activate" subid;
                      LiveView.apply bonsai_app effect
                    in
                    let msg =
                      let open Yojson.Safe in
                      let updates =
                        List.map
                          (fun (component, html) -> (component, `String html))
                          updates
                      in
                      let obj = `Assoc [ ("updates", `Assoc updates) ] in
                      to_string obj
                    in
                    let%lwt () = Dream.send websocket msg in
                    loop ()
              end
            | None ->
                let msg = "error: invalid subscription id: " ^ subid in
                let () =
                  List.iter
                    (fun (x, _) -> Dream.log "%s" x)
                    bonsai_subscriptions
                in
                let%lwt () = Dream.send websocket msg in
                loop ()
            end
          end
        | Ok (Info info) ->
            let msg = "received info: " ^ info in
            let%lwt () = Dream.send websocket msg in
            loop ()
        | Error emsg ->
            let msg = "error: cannot parse message: \"" ^ msg ^ "\": " ^ emsg in
            let%lwt () = Dream.send websocket msg in
            loop ())
  in
  loop ()

(* TODO There is a conceptual gap for the handover from HTTP GET semantics to
   updating via a websocket. The initial render happens over GET, updates over
   the socket. I tried to hand over the state from the initial render to the
   socket. Dream doesn't allow it. So I tried storing things in the session.
   Then got lazy and resorted to re-initializing the state for the websocket
   afresh. This is mostly fine, but it assumes that view and initialization are
   deterministic.

   I think it would be better to re-send the entire state over the websocket on
   initialization (as first update). Or at least verify that the states are
   equal (e.g. by putting a hash of the pre-render state into the websocket
   arguments and verifying it on websocket boot).

   Even if the initial render differs from the initial state of the websocket,
   the initial morphdom update could still synchronize the state.

   Another idea is to not even attempt to have the same HTML pre-render and
   after boot. E.g. the websocket related stuff can just be omitted (e.g.
   pointers to server side effects [e.g. on button click do ...]). Or even
   better, there could be different effects (e.g. POST/redirect/GET instead of
   liveview updates). If I recall correctly; I started this repo with a
   POST/redirect/GET implementation of a counter.

   But this is twisting my brain now. For POST/redirect/GET, the server side
   effects have to be serializable for sure. And if they are serializable, do
   we still need the Weak_ptr things?

   However; it's probably best to start with the weak pointers and websocket
   only semantics. Then think about the fallback to strict HTTP semantics
   later. It's probably a non-goal anyways.
*)

let get_handler req =
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
              let main = main req in
              Dream.websocket (liveview main))
    end
  | _ ->
      let csrf_token = Dream.csrf_token req in
      let main = main req in
      let bonsai_html = LiveView.initial_html main in
      dream_tyxml ~csrf_token bonsai_html

let handler req = get_handler req
let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
