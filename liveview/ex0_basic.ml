open Bonesai.Let_syntax

module Counter = struct
  open Liveview

  module Action = struct
    type t =
      | Increment
      | Decrement
  end

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine
        graph
        ~default_model:start
        ~apply_action:(fun (_ : _ Bonesai.Apply_action_context.t) model -> function
        | Action.Increment -> model + 1
        | Action.Decrement -> model - 1)
    in
    let+ state and+ inject
    and+ id = Liveview.component_id ctx graph
    and+ incr = Liveview.handler_id ctx graph
    and+ decr = Liveview.handler_id ctx graph
    in
    let render ctx =
      let open Html in
      let button label_ action id =
        let handler = id, fun () -> inject action in
        button ~a:[ a_onclick ctx handler ] [ txt label_ ]
      in
      [ button "-1" Action.Decrement decr
      ; txt (Int.to_string state)
      ; button "+1" Action.Increment incr
      ]
    in
    Component.div id render ctx
end

module Input = struct
  open Liveview

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine
        graph
        ~default_model:start
        ~apply_action:(fun (_ : _ Bonesai.Apply_action_context.t) _old new_ -> new_)
    in
    let+ state and+ inject
    and+ id = Liveview.component_id ctx graph
    and+ upd = Liveview.handler_id ctx graph in
    let handler = upd, inject in
    let render ctx =
      let open Html in
      [
        form [
          input ~a:[a_input_type `Text; a_oninput ctx handler; a_value state] ();
        ];
        txt state
      ]
    in
    Component.div id render ctx
end

let main ~n1 ~n2 ~n3 ~s ctx graph =
  let open Liveview in
  let+ one = Counter.component ~start:n1 ctx graph
  and+ two = Counter.component ~start:n2 ctx graph
  and+ three = Counter.component ~start:n3 ctx graph
  and+ four = Input.component ~start:s ctx graph
  and+ id = Liveview.component_id ctx graph
  in
  let open Html in
  let render ctx =
    [ sub_component ctx one
    ; sub_component ctx two
    ; sub_component ctx three
    ; sub_component ctx four
    ]
  in
  Component.div id render ctx

(* read arguments from Dream request *)

let parse_query_s req =
  match Dream.query req "s" with
  | None -> "init"
  | Some s -> s

let parse_query_n1 req =
  match Dream.query req "n1" with
  | None -> 42
  | Some x ->
    match int_of_string_opt x with
    | None -> 42
    | Some x -> x

let parse_query_n2 req =
  match Dream.query req "n2" with
  | None -> 13
  | Some x ->
    match int_of_string_opt x with
    | None -> 13
    | Some x -> x

let parse_query_n3 req =
  match Dream.query req "n3" with
  | None -> 21
  | Some x ->
    match int_of_string_opt x with
    | None -> 21
    | Some x -> x

let main req =
  let n1 = parse_query_n1 req
  and n2 = parse_query_n2 req
  and n3 = parse_query_n3 req
  and s = parse_query_s req in
  main ~n1 ~n2 ~n3 ~s

module Html = Tyxml.Html

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

    function patch_component(id, html) {
      const component = document.getElementById(id);
      if (!component) {
        console.log('error: patch component: component id not found:', id);
        return;
      }
      morphdom(component, html, {
        childrenOnly: true,
        onBeforeElUpdated: (fromEl, toEl) => {
          if (toEl.hasAttribute('data-morphdom-skip')) {
            return false; // skip updating this element
          } else {
            return true;
          };
        },
      });
    };

    socket.onmessage = function (e) {
      var obj = false;
      try {
        obj = JSON.parse(e.data);
      } catch (err) {
        console.log('ws rcv (parse error)', e.data);
        return;
      }
      console.log('ws rcv', obj);
      if (obj && obj[0] === 'Updates') {
        obj[1].forEach(upd => {
          patch_component(upd[0], upd[1]);
        });
      };
    };

    const queue = []

    function send(obj) {
      console.log('ws snd', obj);
      var msg = JSON.stringify(obj);
      if (socket.readyState !== 1) {
        queue.push(msg);
      } else {
        socket.send(msg);
      }
    };

    socket.onopen = function () {
      while (queue.length > 0) {
        socket.send(queue.shift());
      };
    };

    function send_info(text) {
      send(['Info', text])
    };

    function send_event() {
      send(['Event', obj])
    };

    function liveview_handler(name, arg1) {
      return ((id, event) => {
        event.preventDefault();
        event.stopPropagation();

        var obj = [name];
        if (arg1) {
          obj.push(arg1(event));
        }
        send(['Event', id, obj]);
      })
    };

    const liveview_onclick = liveview_handler('OnClick')
    const liveview_oninput = liveview_handler('OnInput', e => e.target.value)

    console.log('js: ready');

    send_info('load 1');
    send_info('load 2');
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

let handler app req =
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
          let app = app req in
          Dream.websocket (Liveview.Dream.run app)
    end
  | _ ->
    let csrf_token = Dream.csrf_token req in
    let app = app req in
    let bonesai_html = Liveview.Dream.prerender app in
    dream_tyxml ~csrf_token bonesai_html

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions (handler main)
