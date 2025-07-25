module Html = Tyxml.Html

module Subscription = struct
  type 'action t =
    | OnClick of 'action
    | OnInput of (string -> 'action)
end

module Context : sig
  type 'action t

  val a_onclick : 'a t -> 'a -> [> `OnClick ] Html.attrib
  val a_oninput : 'a t -> (string -> 'a) -> [> `OnInput ] Html.attrib

  type ('action, 'html) rendering =
    { html : 'html
    ; subscriptions: (int * 'action Subscription.t) list
    }

  val render_with_context: ('a t -> 'state -> 'html) -> 'state -> ('a, 'html) rendering

  val component: ('a t) -> ('b -> 'a)  -> ('b t -> 'state -> 'html) -> 'state -> 'html

end = struct
  type 'action t = (* internal *)
    { mutable subscriptions: (int * 'action Subscription.t) list }

  let fresh () =
    { subscriptions = [] }

  let js_side_event_handler id subscription =
    let name =
      let open Subscription in
      match subscription with
      | OnClick _ -> "onclick"
      | OnInput _ -> "oninput"
    in
    Printf.sprintf "liveview_%s(%i, event)" name id

  let subscribe t s =
    let id = List.length t.subscriptions in
    let () =
      t.subscriptions <- (id, s) :: t.subscriptions
    in
    js_side_event_handler id s

  let a_onclick t a =
    Html.a_onclick (subscribe t (OnClick a))

  let a_oninput t a =
    Html.a_oninput (subscribe t (OnInput a))

  let component t wrap_action render state =
    (* TODO next: sub-component rendering works on initial page load but that's
     * it. Event/subscription routing is not implemented. Also, eventually we
     * want to support two modes; recursive for initial page load and flat to
     * transmit only the updated html of individual components.
     *)
    let html, subscriptions =
      let ctx = fresh () in
      let html = render ctx state in
      html, ctx.subscriptions
    in
    let () =
      List.iter (fun (id, sub) ->
          let add sub =
            t.subscriptions <- (id, sub) :: t.subscriptions
          in
          match sub with
          | Subscription.OnClick a -> add (OnClick (wrap_action a))
          | OnInput f -> add (OnInput (fun s -> wrap_action (f s)))
        ) subscriptions
    in
    html

  type ('action, 'html) rendering =
    { html : 'html
    ; subscriptions: (int * 'action Subscription.t) list
    }

  let render_with_context render state =
    let context = fresh () in
    let html = render context state (* force order of execution *) in
    { html ; subscriptions = context.subscriptions }

end

module type Component = sig
  type state
  type action
  type html

  val apply: state -> action -> state

  val render: action Context.t -> state -> html
end

module Counter = struct
  type state = Counter of int

  let init n = Counter n

  type action = Incr | Decr

  let apply (Counter x) = function
    | Incr -> Counter (x + 1)
    | Decr -> Counter (x - 1)

  type html = [`Div] Html.elt

  let render ctx (Counter x) =
    let open Html in
    let open Context in
    div [
      p [txt (string_of_int x)];
      form [
        input ~a:[a_input_type `Submit; a_onclick ctx Incr; a_value "+ Incr"] ();
        input ~a:[a_input_type `Submit; a_onclick ctx Decr; a_value "- Decr"] ()
        ]
    ]
end

module Input = struct
  type state = Input of string

  let init s = Input s

  type action = Update of string

  module Action = struct
    let update s = Update s
  end

  let apply (Input _s) = function
    | Update (s) -> Input s

  type html = [`Div] Html.elt

  let render ctx (Input s) =
    let open Html in
    let open Context in
    div [
      form ~a:[a_action "."; a_method `Post] [
        input ~a:[a_input_type `Text; a_oninput ctx (Action.update); a_value s] ();
        ]
    ]
end

module App = struct
  type state =
    { counter: Counter.state
    ; input: Input.state
    }

  let init ~n ~s = { counter = Counter.init n; input = Input.init s }

  type action =
    | Counter of Counter.action
    | Input of Input.action

  module Action = struct
    let counter x = Counter x
    let input x = Input x
  end

  type html = [`Div] Html.elt

  let apply state = function
    | Counter a ->
      { state with counter = Counter.apply state.counter a }
    | Input a ->
      { state with input = Input.apply state.input a }

  let render ctx state =
    let open Html in
    let open Context in
    div [
      component ctx Action.counter Counter.render state.counter;
      component ctx Action.input Input.render state.input;
    ]

end

module _ : Component = Counter
module _ : Component = Input
module _ : Component = App

module ExploreBonsaiApi = struct
  open! Core
  module Bonsai = Bonsai.Cont
  open Bonsai.Let_syntax

  module LiveHtml = struct
    type context = (* TODO hide *){ dummy : unit }

    let fresh_context () = { dummy = () }

    include Html

    let a_onclick ~(context: context) inject =
      let _ = context, inject in
      a_onclick ""

    let component ~(context: context) elts =
      let _ = context in
      div ~a:[a_id "liveview-component-0"] elts
  end

  module Counter = struct

    module Action = struct
      type t =
        | Increment
        | Decrement
      [@@deriving sexp_of]
    end

    let component ~context ~start graph : [`Div] Html.elt Bonsai.t =
      let state, inject =
        Bonsai.state_machine0
          graph
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Action.t]
          ~default_model:start
          ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model -> function
          | Action.Increment -> model + 1
          | Action.Decrement -> model - 1)
      in
      let%arr state and inject in
      let open LiveHtml in
      let _ = inject in
      let button label_ action =
        button
          ~a:[ a_onclick ~context (fun () -> inject action) ]
          [ txt label_ ]
      in
      component ~context
        [ button "-1" Action.Decrement
        ; txt (Int.to_string state)
        ; button "+1" Action.Increment
        ]

  end

  let test () =
    let bonsai =
      let context = LiveHtml.fresh_context () in
      Counter.component ~context ~start:42
    and clock =
      let now = Core.Time_ns.now () in
      Bonsai.Time_source.create ~start:now
    in
    let open Bonsai_driver in
    let driver = create ~clock bonsai in
    let () = flush driver in
    let r = result driver in
    let () = trigger_lifecycles driver in
    r


end

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
 * *)

let parse_query_s req =
  match Dream.query req "s" with
  | None -> ""
  | Some s -> s

let parse_query_n req =
  match Dream.query req "n" with
  | None -> 42
  | Some x ->
    match int_of_string_opt x with
    | None -> 42
    | Some x -> x

let initial_state req : App.state =
  let n = parse_query_n req
  and s = parse_query_s req in
  App.init ~n ~s

let reproduce_initial_state = initial_state

(* end state init logic *)

let dream_tyxml ~csrf_token x =
  let js = Printf.sprintf "
    console.log('js: hello js');
    const loc = window.location;
    const tok = '%s';
    var ws_url;
    if (loc.search) {
      ws_url = `//${loc.host}${loc.pathname}${loc.search}&csrf_token=${tok}`;
    } else {
      ws_url = `//${loc.host}${loc.pathname}?csrf_token=${tok}`;
    }
    const socket = new WebSocket(ws_url);

    socket.onmessage = function (e) {
      console.log('ws rcv: ' + e.data);
      var obj = false;
      try {
        obj = JSON.parse(e.data);
      } catch (e) {}
      if (obj && obj.hasOwnProperty('update')) {
        const new_body = document.createElement('body');
        new_body.innerHTML = obj.update;
        morphdom(document.body, new_body);
      }
    };

    const queue = []

    function send_message(m) {
      console.log('ws snd: ' + m);
      if (socket.readyState !== 1) {
        queue.push(m);
      } else {
        socket.send(m);
      }
    };

    socket.onopen = function () {
      while (queue.length > 0) {
        socket.send(queue.shift());
      };
    };

    function liveview_handler(name, arg1) {
      return ((id, event) => {
        event.preventDefault();
        event.stopPropagation();

        var msg = name + '|' + id;
        if (arg1) {
          msg += '|' + arg1(event);
        }
        send_message(msg);
      })
    };

    const liveview_onclick = liveview_handler('onclick')
    const liveview_oninput = liveview_handler('oninput', e => e.target.value)

    console.log('js: ready js');

    send_message('info|load 1');
    send_message('info|load 2');
    " csrf_token
  in
  let html =
    let open Html in
    html
      (head (title (txt "Counter")) [
          script (cdata_script js);
        script ~a:[a_src "https://cdn.jsdelivr.net/npm/morphdom@2.7.5/dist/morphdom-umd.min.js"] (txt "")
        ])
      (body [x])
  in
  let str = Format.asprintf "%a" (Html.pp ()) html in
  Dream.html str

module Message = struct
  open Angstrom

  type event =
    | OnClick
    | OnInput of string

  type t =
    | Event of int * event
    | Info of string

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun digits ->
    match int_of_string_opt digits with
    | Some i -> return i
    | None -> fail "Invalid integer"

  let onclick =
    string "onclick|" *> integer >>| fun i ->
    Event (i, OnClick)

  let oninput =
    string "oninput|" *> integer >>= fun i ->
    char '|' *> take_while (fun _ -> true) >>| fun s ->
    Event (i, OnInput s)

  let info =
    string "info|" *> take_while (fun _ -> true) >>| fun s -> Info s

  let t_parser =
    choice [onclick; oninput; info]

  let parse_t =
    parse_string ~consume:Consume.All t_parser
end

let action_of_event_and_subscription event subscription =
  match event, subscription with
  | Message.OnClick, Subscription.OnClick a -> Some a
  | OnInput s, OnInput f -> Some (f s)
  | _ -> None

let liveview subscriptions state websocket =
  let%lwt () = Dream.send websocket "hello socket" in
  let rec loop subscriptions state =
    match%lwt Dream.receive websocket with
    | None -> Lwt.return ()
    | Some msg ->
        match Message.parse_t msg with
          (* TODO Tracking last subscriptions might not be enough. Assume
           * client triggers second event before the server can handle the
           * first. This can lead to routing errors. One solution can be to
           * maintain a ring buffer of recent subscriptions. Each event would
           * include the lowest id still relevant; the event handler would
           * purge all subscriptions not relevant any more.
           *)
        | Ok (Event (subid, event)) ->
          let msg = "received event: " ^ msg in
          let%lwt () = Dream.send websocket msg in
          begin
            match List.assoc_opt subid subscriptions with
            | None ->
              let msg = "error: invalid subscription id: " ^ string_of_int subid in
              let%lwt () = Dream.send websocket msg in
              loop subscriptions state
            | Some sub ->
              begin
                match action_of_event_and_subscription event sub with
                | None ->
                  let msg = "error: event/subscription mismatch: " ^ msg in
                  let%lwt () = Dream.send websocket msg in
                  loop subscriptions state
                | Some action ->
                  let state = App.apply state action in
                  let Context.{html; subscriptions} =
                    Context.render_with_context App.render state
                  in
                  let msg =
                    let open Yojson.Safe in
                    let upd = Format.asprintf "%a" (Html.pp_elt ()) html in
                    let obj = `Assoc [("update", `String upd)] in
                    to_string obj
                  in
                  let%lwt () = Dream.send websocket msg in
                  loop subscriptions state
              end
          end
        | Ok (Info info) ->
          let msg = "received info: " ^ info in
          let%lwt () = Dream.send websocket msg in
          loop subscriptions state
        | Error emsg ->
          let msg = "error: cannot parse message: \""
                    ^ msg ^ "\": " ^ emsg
          in
          let%lwt () = Dream.send websocket msg in
          loop subscriptions state
  in loop subscriptions state

let get_handler req =
  match Dream.header req "Upgrade" with
  | Some "websocket" ->
    begin
      match Dream.query req "csrf_token" with
      | None -> Dream.empty `Unauthorized
      | Some token ->
        match%lwt Dream.verify_csrf_token req token with
        | `Expired _ -> Dream.empty `Forbidden
        | `Wrong_session -> Dream.empty `Forbidden
        | `Invalid -> Dream.empty `Unauthorized
        | `Ok ->
          let state = reproduce_initial_state req in
          let rendered =
            Context.render_with_context App.render state
          in
          Dream.websocket (liveview rendered.subscriptions state)
    end
  | _ ->
    let csrf_token = Dream.csrf_token req in
    let state = initial_state req in
    let rendered =
      Context.render_with_context App.render state
    in
    dream_tyxml ~csrf_token rendered.html

let handler req =
  get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
