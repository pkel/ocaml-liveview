module Context = struct
  type 'a t =
    { instance_id : string
    ; string_of_action: 'a -> string
    }

  let a_formaction t a =
    Tyxml_html.a_formaction (".?action=" ^ (t.string_of_action a) ^ "&instance=" ^ (Dream.to_base64url t.instance_id))
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

  let init () = Counter 42

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
        input ~a:[a_input_type `Submit; a_formaction ctx Incr; a_value "+ Incr"] ();
        input ~a:[a_input_type `Submit; a_formaction ctx Decr; a_value "- Decr"] ()
        ]
    ]
end

module C : Component = Counter

let dream_tyxml x =
  let js = "
    console.log('js: hello js');
    const socket = new WebSocket('//' + window.location.host + window.location.pathname);

    socket.onmessage = function (e) {
      console.log('ws: ' + e.data);
    };

    const queue = []

    function send_message(m) {
      if (socket.readyState !== 1) {
        queue.push(m);
      } else {
        socket.send(msg);
      }
    };

    socket.onopen = function () {
      while (queue.length > 0) {
        socket.send(queue.shift());
      };
    };

    function send_action(action) {
      send_message('action: ' + action);
    };

    console.log('js: ready js');

    send_action('load 1');
    send_action('load 2');
    "
  in
  let body =
    let open Tyxml.Html in
    body [script (cdata_script js); x]
  in
  let str = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) body in
  Dream.html str

let get_state req =
  (* TODO maintain one state per window / instance_id *)
  match Dream.session_field req "state" with
  | None -> Counter.init ()
  | Some str -> Marshal.from_string str 0

let put_state req state =
  Dream.set_session_field req "state" (Marshal.to_string state [])

let action_handler req action =
  match Counter.action_of_string action with
    | Some action ->
        let state = get_state req in
        let state' = Counter.apply state action in
        let%lwt () = put_state req state' in
        Dream.redirect req "."
    | None ->
        Dream.redirect req "."

let liveview _state websocket =
  let%lwt () = Dream.send websocket "hello socket" in
  let rec loop () =
    match%lwt Dream.receive websocket with
    | None -> Lwt.return ()
    | Some msg ->
      let%lwt () = Dream.send websocket ("echo: " ^ msg) in
      loop ()
  in loop ()

let get_handler req =
  let state = get_state req in
  match Dream.header req "Upgrade" with
  | Some "websocket" ->
    (* TODO check CSRF tocken *)
    Dream.websocket (liveview state)
  | _ ->
    (* TODO reuse instance id if given *)
    (* TODO I guess we won't need the instance id; just do all interactivity over the web socket *)
    let context = Context.{ instance_id = Dream.random 16 ; string_of_action = Counter.string_of_action } in
    dream_tyxml (Counter.render context state)

let handler req =
  match Dream.query req "action" with
  | Some str -> action_handler req str
  | None -> get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
