type _ t =
  | Constant : 'a -> 'a t
  | Both : 'a t * 'b t -> ('a * 'b) t
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      }
      -> 'a t
  | Map :
      { t : 'a t
      ; f : 'a -> 'b
      }
      -> 'b t
  | Map2 :
      { t1 : 't1 t
      ; t2 : 't2 t
      ; f : 't1 -> 't2 -> 'r
      }
      -> 'r t

let physical_eq a b = a == b
let default_eq = physical_eq

let rec eval : type a. ?equal: (a -> a -> bool) -> a t -> a React.signal =
  fun ?(equal = default_eq) value ->
  match value with
  | Cutoff { t; equal } -> eval ~equal t
  | Constant x -> React.S.const x
  | Both (t1, t2) ->
      React.S.bind ~eq:equal (eval t1) (fun x1 ->
        React.S.map ~eq:physical_eq (fun x2 ->
          (x1, x2)
        ) (eval t2)
      )
  | Map { t; f } ->
      React.S.map ~eq:equal f (eval t)
  | Map2 { t1; t2; f } ->
      React.S.bind ~eq:equal (eval t1) (fun x1 ->
        React.S.map ~eq:physical_eq (fun x2 ->
          f x1 x2
        ) (eval t2)
      )
;;

let eval value = eval ~equal:default_eq value
