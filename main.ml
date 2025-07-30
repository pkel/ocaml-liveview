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
    ; subscriptions: (string * 'action Subscription.t) list
    }

  val render_with_context: ('a t -> 'state -> 'html) -> 'state -> ('a, 'html) rendering

  val component: ('a t) -> ('b -> 'a)  -> ('b t -> 'state -> 'html) -> 'state -> 'html

end = struct
  type 'action t = (* internal *)
    { mutable subscriptions: (string * 'action Subscription.t) list }

  let fresh () =
    { subscriptions = [] }

  let js_side_event_handler id subscription =
    let name =
      let open Subscription in
      match subscription with
      | OnClick _ -> "onclick"
      | OnInput _ -> "oninput"
    in
    Printf.sprintf "liveview_%s('%s', event)" name id

  let subscribe t s =
    let id = Printf.sprintf "pre-bonsai-%i" (List.length t.subscriptions) in
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
    ; subscriptions: (string * 'action Subscription.t) list
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

  module LiveView : sig
    type 'a handler = 'a -> unit Bonsai.Effect.t

    type subscription = (* TODO hide implementation *)
      | OnClick of unit handler
      | OnInput of string handler

    type context

    type 'a component

    val component: string -> (context -> [< Html_types.div_content_fun ] Html.elt list) -> [> ` Div ] component
    (* TODO first argument is component's id (Bonsai.path_id). Hide this technicality here *)

    type 'a app = 'a component Bonsai_driver.t (* TODO hide, expose variants of Bonsai_driver.* functions instead *)

    val app: (Bonsai.graph -> 'a component Bonsai.t) -> 'a app

    val html: 'a app -> 'a Html.elt

    val subscriptions: 'a app -> (string * subscription) list (* TODO remove when hiding app type *)

    module Html : sig
      include module type of Html

      val a_onclick : context -> unit handler ->  [> `OnClick ] attrib
      val a_oninput : context -> string handler ->  [> `OnInput ] attrib

      val sub_component : context -> 'a component -> 'a Html.elt
    end

  end = struct
    type 'a handler = 'a -> unit Bonsai.Effect.t

    type subscription =
      | OnClick of unit handler
      | OnInput of string handler

    type context = {
      component_id: string;
      mutable subscriptions : (string * subscription) list
    }

    let add_subscription ctx id sub =
        ctx.subscriptions <- (id, sub) :: ctx.subscriptions

    type 'a component = {
      html :  'a Html.elt;
      subscriptions :  (string * subscription) list
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

      let a_onclick (ctx: context) handler =
        let len = List.length ctx.subscriptions in
        let id = Printf.sprintf "%s:%i" ctx.component_id len in
        let sub = OnClick handler in
        add_subscription ctx id sub;
        a_onclick (js_side_event_handler id sub)

      let a_oninput (ctx: context) handler =
        let len = List.length ctx.subscriptions in
        let id = Printf.sprintf "%s:%i" ctx.component_id len in
        let sub = OnInput handler in
        add_subscription ctx id sub;
        a_oninput (js_side_event_handler id sub)

      let sub_component ctx c =
        List.iter ~f:(fun (id, sub) -> add_subscription ctx id sub) c.subscriptions;
        c.html

    end

    let component id render =
      let ctx : context = { component_id = id; subscriptions = [] } in
      let elts = render ctx in
      let html =
        let open Html in
        div ~a:[a_id ctx.component_id] elts
      in
      { html = html
      ; subscriptions = ctx.subscriptions }

    type 'a app = 'a component Bonsai_driver.t (* TODO hide *)

    let app bonsai =
      let clock =
        let now = Core.Time_ns.now () in
        Bonsai.Time_source.create ~start:now
      in
      Bonsai_driver.create ~clock bonsai

    let html x =
      let r = Bonsai_driver.result x in
      r.html

    let subscriptions x =
      let r = Bonsai_driver.result x in
      r.subscriptions

  end

  module Counter = struct
    open LiveView

    module Action = struct
      type t =
        | Increment
        | Decrement
      [@@deriving sexp_of]
    end

    let component ~start graph : [> `Div ] component Bonsai.t =
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
      and id = Bonsai.path_id graph
      in
      let%arr state and inject and id in
      let render ctx =
        let open Html in
        let button label_ action =
          let handler () = inject action in
          button ~a:[ a_onclick ctx handler ] [ txt label_ ]
        in
        [ button "-1" Action.Decrement
        ; txt (Int.to_string state)
        ; button "+1" Action.Increment
        ]
      in
      component id render
  end

  module Input = struct
    open LiveView

    let component ~start graph : [> `Div ] component Bonsai.t =
      let state, inject =
        Bonsai.state_machine0
          graph
          ~sexp_of_model:Sexp.of_string
          ~equal:String.equal
          ~sexp_of_action:Sexp.of_string
          ~default_model:start
          ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _old new_ -> new_)
      and id = Bonsai.path_id graph in
      let%arr state and inject and id in
      let render ctx =
        let open Html in
        [
          form ~a:[a_action "."; a_method `Post] [
            input ~a:[a_input_type `Text; a_oninput ctx inject; a_value state] ();
            ]
        ]
      in component id render
  end

  let main graph =
    let open LiveView in
    let%sub one = Counter.component ~start:42 graph in
    let%sub two = Counter.component ~start:13 graph in
    let%sub three = Counter.component ~start:21 graph in
    let%sub four = Input.component ~start:"hello" graph in
    let%sub id = Bonsai.path_id graph in
    let%arr one and two and three and four and id in
    let open Html in
    let render ctx =
      [ sub_component ctx one
      ; sub_component ctx two
      ; sub_component ctx three
      ; sub_component ctx four
      ]
    in
    component id render

  let test () =
    let driver = LiveView.app main in
    let open Bonsai_driver in
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
  (* TODO use json as message format *)

  open Angstrom

  type event =
    | OnClick
    | OnInput of string

  type t =
    | Event of string * event
    | Info of string

  let id = take_while (fun c -> c <> '|')

  let onclick =
    string "onclick|" *> id >>| fun i ->
    Event (i, OnClick)

  let oninput =
    string "oninput|" *> id >>= fun i ->
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

let effect_of_event_and_sub event sub =
  match event, sub with
  | Message.OnClick, ExploreBonsaiApi.LiveView.OnClick f -> Some (f ())
  | OnInput s, OnInput f -> Some (f s)
  | _ -> None

let liveview subscriptions state websocket =
  let bonsai_app = ExploreBonsaiApi.(LiveView.app main) in
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
                  let bonsai_html = ExploreBonsaiApi.LiveView.html bonsai_app in
                  let html =
                    Html.div [ html; Html.hr (); bonsai_html ]
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
            | None ->
              let bonsai_subscriptions = ExploreBonsaiApi.LiveView.subscriptions bonsai_app in
              begin match List.assoc_opt subid bonsai_subscriptions with
              | Some sub ->
                begin match effect_of_event_and_sub event sub with
                | None ->
                  let msg = "error: event/subscription mismatch: " ^ msg in
                  let%lwt () = Dream.send websocket msg in
                  loop subscriptions state
                | Some effect ->
                  let () =
                    (* TODO hide Bonsai_driver logic here *)
                    let open Bonsai_driver in
                    schedule_event bonsai_app effect;
                    flush bonsai_app
                  in
                  let bonsai_html = ExploreBonsaiApi.LiveView.html bonsai_app in
                  let Context.{html; subscriptions} =
                    Context.render_with_context App.render state
                  in
                  let html =
                    Html.div [ html; Html.hr (); bonsai_html ]
                  in
                  let msg =
                    let open Yojson.Safe in
                    let upd = Format.asprintf "%a" (Html.pp_elt ()) html in
                    let obj = `Assoc [("update", `String upd)] in
                    to_string obj
                  in
                  let%lwt () = Dream.send websocket msg in
                  let () =
                    (* TODO hide driver logic here *)
                    Bonsai_driver.trigger_lifecycles bonsai_app
                  in
                  loop subscriptions state
                end
              | None ->
                let msg = "error: invalid subscription id: " ^ subid in
                let () = List.iter (fun (x, _) -> Dream.log "%s" x) bonsai_subscriptions in
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
      let pre_bonsai =
        (Context.render_with_context App.render state).html
      and bonsai_app = ExploreBonsaiApi.(LiveView.app main)
      in
      let bonsai_html = ExploreBonsaiApi.LiveView.html bonsai_app in
      let open Html in
      div [pre_bonsai; hr (); bonsai_html ]
    in
    dream_tyxml ~csrf_token rendered

let handler req =
  get_handler req

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions handler
