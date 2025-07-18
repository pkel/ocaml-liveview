module Context = struct
  type 'action subscription = (* internal *)
    | OnClick of 'action
    | OnInput of (string -> 'action)

  type 'action t = (* internal *)
    { mutable subscriptions: (int * 'action subscription) list }

  let fresh () =
    { subscriptions = [] }

  let js_side_event_handler id subscription =
    let name =
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
    Tyxml_html.a_onclick (subscribe t (OnClick a))

  let a_oninput t a =
    Tyxml_html.a_oninput (subscribe t (OnInput a))

end

module type Component = sig
  type state
  type action
  type out

  val action_of_string: string -> action option
  val string_of_action: action -> string

  val apply: state -> action -> state

  val render: action Context.t -> state -> out Tyxml_html.elt
end

module Counter = struct
  type state = Counter of int

  let init n = Counter n

  type action = Incr | Decr

  let action_of_string = function
    | "incr" -> Some Incr
    | "decr" -> Some Decr
    | _ -> None

  let string_of_action = function
    | Incr -> "incr"
    | Decr -> "decr"

  let apply (Counter x) = function
    | Incr -> Counter (x + 1)
    | Decr -> Counter (x - 1)

  type out = [`Div]

  let render ctx (Counter x) =
    let open Tyxml.Html in
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

  module ActionParser = struct
    open Angstrom

    let update_parser =
      string "update|" *> take_while (fun c -> c <> '\n') >>| fun s -> Update s

    let action_parser =
      choice [update_parser]

    let parse s =
      parse_string ~consume:Consume.All action_parser s
  end

  let action_of_string s =
    match ActionParser.parse s with
    | Error _ -> None
    | Ok a -> Some a

  let string_of_action = function
    | Update s -> "update|" ^ s

  let apply (Input _s) = function
    | Update (s) -> Input s

  type out = [`Div]

  let render ctx (Input s) =
    let open Tyxml.Html in
    let open Context in
    div [
      form ~a:[a_action "."; a_method `Post] [
        input ~a:[a_input_type `Text; a_oninput ctx (Action.update); a_value s] ();
        ]
    ]
end

module _ : Component = Counter
module _ : Component = Input

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

let parse_query_n req =
  match Dream.query req "init" with
  | None -> 42
  | Some x ->
    match int_of_string_opt x with
    | None -> 42
    | Some x -> x

let initial_state req : Counter.state =
  parse_query_n req |> Counter.init

let reproduce_initial_state req : Counter.state =
  parse_query_n req |> Counter.init

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
          msg += '|' + arg1;
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
    let open Tyxml.Html in
    html
      (head (title (txt "Counter")) [
          script (cdata_script js);
        script ~a:[a_src "https://cdn.jsdelivr.net/npm/morphdom@2.7.5/dist/morphdom-umd.min.js"] (txt "")
        ])
      (body [x])
  in
  let str = Format.asprintf "%a" (Tyxml.Html.pp ()) html in
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
  | Message.OnClick, Context.OnClick a -> Some a
  | OnInput s, OnInput f -> Some (f s)
  | _ -> None

let liveview init_context state websocket =
  let%lwt () = Dream.send websocket "hello socket" in
  let rec loop (last_context : Counter.action Context.t) state =
    match%lwt Dream.receive websocket with
    | None -> Lwt.return ()
    | Some msg ->
        match Message.parse_t msg with
          (* TODO Tracking last_context might not be enough. Assume client triggers
           * second event before the server can handle the first. This can lead
           * to routing errors. One solution can be to maintain a ring buffer
           * of recent subscriptions. Each event would include the lowest id
           * still relevant; the event handler would purge all subscriptions
           * not relevant any more.
           *)
        | Ok (Event (subid, event)) ->
          let msg = "received event: " ^ msg in
          let%lwt () = Dream.send websocket msg in
          begin
            match List.assoc_opt subid last_context.subscriptions with
            | None ->
              let msg = "error: invalid subscription id: " ^ string_of_int subid in
              let%lwt () = Dream.send websocket msg in
              loop last_context state
            | Some sub ->
              begin
                match action_of_event_and_subscription event sub with
                | None ->
                  let msg = "error: event/subscription mismatch: " ^ msg in
                  let%lwt () = Dream.send websocket msg in
                  loop last_context state
                | Some action ->
                  let state = Counter.apply state action in
                  let context = Context.fresh () in
                  let html = Counter.render context state in
                  let msg =
                    let open Yojson.Safe in
                    let upd = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html in
                    let obj = `Assoc [("update", `String upd)] in
                    to_string obj
                  in
                  let%lwt () = Dream.send websocket msg in
                  loop context state
              end
          end
        | Ok (Info info) ->
          let msg = "received info: " ^ info in
          let%lwt () = Dream.send websocket msg in
          loop last_context state
        | Error emsg ->
          let msg = "error: cannot parse message: \""
                    ^ msg ^ "\": " ^ emsg
          in
          let%lwt () = Dream.send websocket msg in
          loop last_context state
  in loop init_context state

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
          let context = Context.fresh () in
          (* TODO footgun alarm; the mutable context should be hidden; do
           * expose a render_with_context function instead, which returns
           * rendered html and a list of subscriptions. *)
          let _ = Counter.render context state in
          Dream.websocket (liveview context state)
    end
  | _ ->
    let csrf_token = Dream.csrf_token req in
    let state = initial_state req in
    let context = Context.fresh () in
    (* TODO footgun alarm; the mutable context should be hidden; do
     * expose a render_with_context function instead, which returns
     * rendered html and a list of subscriptions. *)
    dream_tyxml ~csrf_token (Counter.render context state)

let handler req =
  get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
