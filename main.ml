module Context = struct
  type 'a t =
    { string_of_action: 'a -> string }

  let a_onclick t a =
    let js =
      Printf.sprintf
        "liveview_action(event, \"%s\")"
        (t.string_of_action a)
    in
    Tyxml_html.a_onclick js
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
      form ~a:[a_action "."; a_method `Post] [
        input ~a:[a_input_type `Submit; a_onclick ctx Incr; a_value "+ Incr"] ();
        input ~a:[a_input_type `Submit; a_onclick ctx Decr; a_value "- Decr"] ()
        ]
    ]
end

module C : Component = Counter

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

    function send_action(action) {
      send_message('action|' + action);
    };

    function liveview_action(event, action) {
      event.preventDefault();
      event.stopPropagation();
      send_action(action);
    };

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

  type t =
    | Action of string
    | Info of string

  let action_parser =
    string "action|" *> take_while (fun c -> c <> '\n') >>| fun s -> Action s

  let info_parser =
    string "info|" *> take_while (fun c -> c <> '\n') >>| fun s -> Info s

  let t_parser =
    choice [action_parser; info_parser]

  let parse msg =
    parse_string ~consume:Consume.All t_parser msg
end

let liveview context state websocket =
  let%lwt () = Dream.send websocket "hello socket" in
  let rec loop state =
    match%lwt Dream.receive websocket with
    | None -> Lwt.return ()
    | Some msg ->
        match Message.parse msg with
        | Ok (Action action) ->
          begin
          match Counter.action_of_string action with
          | Some action ->
            let state = Counter.apply state action in
            let html = Counter.render context state in
            let msg =
              let open Yojson.Safe in
              let upd = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html in
              let obj = `Assoc [("update", `String upd)] in
              to_string obj
            in
            let%lwt () = Dream.send websocket msg in
            loop state
          | None ->
            let msg = "error: unknown action: \""
                      ^ action ^ "\""
            in
            let%lwt () = Dream.send websocket msg in
            loop state
          end
        | Ok (Info info) ->
          let msg = "received info: " ^ info in
          let%lwt () = Dream.send websocket msg in
          loop state
        | Error emsg ->
          let msg = "error: cannot parse message: \""
                    ^ msg ^ "\": " ^ emsg
          in
          let%lwt () = Dream.send websocket msg in
          loop state
  in loop state

let get_handler req =
  let context = Context.{ string_of_action = Counter.string_of_action } in
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
          Dream.websocket (liveview context state)
    end
  | _ ->
    let csrf_token = Dream.csrf_token req in
    let state = initial_state req in
    dream_tyxml ~csrf_token (Counter.render context state)

let handler req =
  get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
