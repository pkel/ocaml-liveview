open Bonesai.Let_syntax

module Counter = struct
  open Liveview

  module Action = struct
    type t = Increment | Decrement
  end

  let apply_action (_ : _ Bonesai.Apply_action_context.t) model action =
    match (action : Action.t) with
    | Increment -> model + 1
    | Decrement -> model - 1

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start ~apply_action
    in
    let+ state = state
    and+ inject = inject
    and+ id = Liveview.component_id ctx graph
    and+ incr = Liveview.handler_id ctx graph
    and+ decr = Liveview.handler_id ctx graph in
    let render ctx =
      let open Html in
      let button label_ action id =
        let handler = (id, fun () -> inject action) in
        button ~a:[ a_onclick ctx handler ] [ txt label_ ]
      in
      [
        button "-1" Action.Decrement decr;
        txt (Int.to_string state);
        button "+1" Action.Increment incr;
      ]
    in
    Component.div id render ctx
end

module Input = struct
  open Liveview

  let component ~start ctx graph : [> `Div ] component Bonesai.t =
    let state, inject =
      Bonesai.state_machine graph ~default_model:start
        ~apply_action:(fun (_ : _ Bonesai.Apply_action_context.t) _old new_ ->
          new_)
    in
    let+ state = state
    and+ inject = inject
    and+ id = Liveview.component_id ctx graph
    and+ upd = Liveview.handler_id ctx graph in
    let handler = (upd, inject) in
    let render ctx =
      let open Html in
      [
        form
          [
            input
              ~a:[ a_input_type `Text; a_oninput ctx handler; a_value state ]
              ();
          ];
        txt state;
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
  and+ id = Liveview.component_id ctx graph in
  let open Html in
  let render ctx =
    [
      sub_component ctx one;
      sub_component ctx two;
      sub_component ctx three;
      sub_component ctx four;
    ]
  in
  Component.div id render ctx

(* read arguments from Dream request *)

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

module Html = Tyxml.Html

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
      \      if (!component) {\n\
      \        console.log('error: patch component: component id not found:', \
       id);\n\
      \        return;\n\
      \      }\n\
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
      \        console.log('ws rcv (parse error)', e.data);\n\
      \        return;\n\
      \      }\n\
      \      console.log('ws rcv', obj);\n\
      \      if (obj && obj[0] === 'Updates') {\n\
      \        obj[1].forEach(upd => {\n\
      \          patch_component(upd[0], upd[1]);\n\
      \        });\n\
      \      };\n\
      \    };\n\n\
      \    const queue = []\n\n\
      \    function send(obj) {\n\
      \      console.log('ws snd', obj);\n\
      \      var msg = JSON.stringify(obj);\n\
      \      if (socket.readyState !== 1) {\n\
      \        queue.push(msg);\n\
      \      } else {\n\
      \        socket.send(msg);\n\
      \      }\n\
      \    };\n\n\
      \    socket.onopen = function () {\n\
      \      while (queue.length > 0) {\n\
      \        socket.send(queue.shift());\n\
      \      };\n\
      \    };\n\n\
      \    function send_info(text) {\n\
      \      send(['Info', text])\n\
      \    };\n\n\
      \    function send_event() {\n\
      \      send(['Event', obj])\n\
      \    };\n\n\
      \    function liveview_handler(name, arg1) {\n\
      \      return ((id, event) => {\n\
      \        event.preventDefault();\n\
      \        event.stopPropagation();\n\n\
      \        var obj = [name];\n\
      \        if (arg1) {\n\
      \          obj.push(arg1(event));\n\
      \        }\n\
      \        send(['Event', id, obj]);\n\
      \      })\n\
      \    };\n\n\
      \    const liveview_onclick = liveview_handler('OnClick')\n\
      \    const liveview_oninput = liveview_handler('OnInput', e => \
       e.target.value)\n\n\
      \    console.log('js: ready');\n\n\
      \    send_info('load 1');\n\
      \    send_info('load 2');\n\
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

let handler app req =
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
              Dream.websocket (Liveview.Dream.run app))
    end
  | _ ->
      let csrf_token = Dream.csrf_token req in
      let app = app req in
      let bonesai_html = Liveview.Dream.prerender app in
      dream_tyxml ~csrf_token bonesai_html

let () = Dream.run @@ Dream.logger @@ Dream.memory_sessions (handler main)
