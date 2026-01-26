open Ppxlib
module Ast_builder = Ast_builder.Default

type ty = Unit | String

type ctx =
  { loc: Astlib.Location.t
  ; mutable captured_values: string list
  ; mutable event_handlers: (string * expression * ty) list }

let fresh_ctx loc = {loc; captured_values= []; event_handlers= []}

let pexp_ident' ~loc str =
  Ast_builder.pexp_ident ~loc {txt= Astlib.Longident.parse str; loc}

let component_visitor =
  (* scan, apply, and resolve [%a ..] and [%v ..] extensions within
     [%component <container> ... ] *)
  let get_lident = function
    | {pexp_desc= Pexp_ident {txt= Lident str; _}; _} ->
        Ok str
    | {pexp_loc; _} ->
        Error pexp_loc
  in
  object
    inherit ['ctx] Ast_traverse.map_with_context as super

    method! expression ctx expr =
      match expr with
      | [%expr [%a [%e? action_expr]]] ->
          let var = List.length ctx.event_handlers |> Printf.sprintf "ev%d" in
          (* TODO de-duplicate event handlers *)
          ctx.event_handlers <- (var, action_expr, Unit) :: ctx.event_handlers ;
          pexp_ident' ~loc:action_expr.pexp_loc var
      | {pexp_desc= Pexp_extension ({txt= "a"; _}, _payload); pexp_loc= loc; _}
        ->
          Ast_builder.pexp_extension ~loc
          @@ Location.error_extensionf ~loc
               "the %%a extension expects one arguments: [%%a <action>]"
      | [%expr [%astring [%e? action_expr]]] ->
          let var = List.length ctx.event_handlers |> Printf.sprintf "ev%d" in
          (* TODO de-duplicate event handlers *)
          ctx.event_handlers <- (var, action_expr, String) :: ctx.event_handlers ;
          pexp_ident' ~loc:action_expr.pexp_loc var
      | { pexp_desc= Pexp_extension ({txt= "astring"; _}, _payload)
        ; pexp_loc= loc
        ; _ } ->
          Ast_builder.pexp_extension ~loc
          @@ Location.error_extensionf ~loc
               "the %%astring extension expects one arguments: [%%astring \
                <action>]"
      | [%expr [%v [%e? expr]]] -> (
        (* match [%v <expr>] *)
        match get_lident expr with
        | Ok str ->
            let () =
              if not (List.mem str ctx.captured_values) then
                ctx.captured_values <- str :: ctx.captured_values
            in
            expr
        | Error loc ->
            Ast_builder.pexp_extension ~loc
            @@ Location.error_extensionf ~loc
                 "the %%v extension expects one arguments: [%%v <value>]" )
      | {pexp_desc= Pexp_extension ({txt= "v"; _}, _payload); pexp_loc= loc; _}
        ->
          Ast_builder.pexp_extension ~loc
          @@ Location.error_extensionf ~loc
               "the %%v extension expects one arguments: [%%v <value>]"
      | _ ->
          super#expression ctx expr
  end

let pexp_Liveview_Component_lident ~loc ident =
  pexp_ident' ~loc ("Liveview.Component." ^ ident)

let pexp_Liveview_Component_argn ~loc n =
  pexp_Liveview_Component_lident ~loc (Printf.sprintf "arg%d" n)

(* TODO the N in argN is capped; find a non-capped solution *)

let expand_component ~ctxt container args =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match args with
  | [(Nolabel, expr)] ->
      let ctx = fresh_ctx loc in
      let elements = component_visitor#expression ctx expr in
      let render_params =
        ctx.captured_values
        @ List.rev_map (fun (x, _, _) -> x) ctx.event_handlers
      in
      let render_fn_expr =
        let body = [%expr [%e elements]] in
        match render_params with
        | [] ->
            body
        | params ->
            let open Ast_builder in
            let params =
              List.map
                (fun txt ->
                  pparam_val ~loc Nolabel None (ppat_var ~loc {loc; txt}) )
                params
            in
            pexp_function ~loc params None (Pfunction_body body)
      and component_expr =
        let fn = pexp_Liveview_Component_argn ~loc (List.length render_params)
        and static_args0 =
          [(Nolabel, pexp_Liveview_Component_lident ~loc container)]
        and dyn_args =
          List.map (fun x -> (Nolabel, pexp_ident' ~loc x)) render_params
        and static_args1 =
          [(Nolabel, [%expr render]); (Nolabel, [%expr graph])]
        in
        Ast_builder.pexp_apply ~loc fn (static_args0 @ dyn_args @ static_args1)
      and with_event_handlers =
        match ctx.event_handlers with
        | [] ->
            Fun.id
        | event_handlers ->
            let open Ast_builder in
            let bindings =
              List.map
                (fun (var, expr, ty) ->
                  let expr =
                    match ty with
                    | Unit ->
                        [%expr Liveview.event_handler to_task [%e expr] graph]
                    | String ->
                        [%expr
                          Liveview.string_event_handler to_task [%e expr] graph]
                  in
                  value_binding ~loc ~pat:(ppat_var ~loc {txt= var; loc}) ~expr )
                event_handlers
            in
            pexp_let ~loc Nonrecursive bindings
      in
      with_event_handlers
        [%expr
          (* TODO event_handler bindings here *)
          let render = [%e render_fn_expr] in
          [%e component_expr]]
  | _ ->
      (* TODO improve error message *)
      Ast_builder.pexp_extension ~loc
      @@ Location.error_extensionf ~loc "invalid use of %%component extension"

let component_ext =
  Extension.V3.declare "component" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_apply (pexp_ident (lident __)) __))
    expand_component

let rules = [Ppxlib.Context_free.Rule.extension component_ext]

let () = Driver.register_transformation ~rules "liveview"
