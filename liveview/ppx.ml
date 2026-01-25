open Ppxlib

type ctx = { mutable n_events : int; mutable n_values : int }

let visitor =
  let single_expr_identity_ext ~loc expr =
    ({ txt = "__single_expr_identity"; loc }, PStr [%str [%e expr]])
  in
  object
    inherit ['ctx] Ast_traverse.map_with_context as super

    method! extension ctx ext =
      match ext with
      | { txt = "a"; loc }, PStr [%str [%e? expr]] ->
          let expr = super#expression ctx expr in
          ctx.n_events <- ctx.n_events + 1;
          single_expr_identity_ext ~loc expr
      | { txt = "v"; loc }, PStr [%str [%e? expr]] ->
          let expr = super#expression ctx expr in
          ctx.n_values <- ctx.n_values + 1;
          single_expr_identity_ext ~loc expr
      | _ -> super#extension ctx ext
  end

let expand_component ~ctxt container args =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match args with
  | [ (Nolabel, expr) ] ->
      let ctx = { n_events = 0; n_values = 0 } in
      let elements = visitor#expression ctx expr in
      let i n =
        Ast_builder.Default.pexp_constant ~loc
          (Pconst_integer (string_of_int n, None))
      in
      (* TODO the generated code does not work obviously; this is only to
         demonstrate that we can scan the elements [expr] for %a and %v
         extensions and do something useful with them

         run `dune describe pp liveview/ex0_basic.ml` to see the intermediary working state
         *)
      [%expr
        (n_events [%e i ctx.n_events])
          (n_values [%e i ctx.n_values])
          Html.([%e container] [%e elements])]
  | _ -> assert false

let expand_identity ~ctxt:_ expr = expr

let component_ext =
  Extension.V3.declare "component" Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_apply __ __))
    expand_component

let identity_ext =
  Extension.V3.declare "__single_expr_identity" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_identity

let rules =
  [
    Ppxlib.Context_free.Rule.extension component_ext;
    Ppxlib.Context_free.Rule.extension identity_ext;
  ]

let () = Driver.register_transformation ~rules "liveview"
