type _ t =
  | Constant : 'a -> 'a t
  | Signal: 'a React.signal -> 'a t
  | Cutoff :
      { t : 'a t
      ; equal : 'a -> 'a -> bool
      }
      -> 'a t
  | Both : 'a t * 'b t -> ('a * 'b) t
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

let phys_equal a b = a == b

let rec eval : type a. ?equal: (a -> a -> bool) -> a t -> a React.signal =
  fun ?(equal = phys_equal) value ->
  match value with
  | Cutoff { t = Signal s; equal } ->
      (* We want to apply a cutoff to an existing signal. This requires a new
         new signal. *)
      React.S.map ~eq:equal Fun.id s
  | Cutoff { t ; equal } ->
      (* Apply the custom cutoff/equal on the next recursive eval step. *)
      eval ~equal t
  | Constant x ->
      (* We have no way to apply the requested cutoff/equal here but as the
         signal is constant we don't have to. *)
      React.S.const x
  | Signal s ->
      (* There's no way to apply the requested cutoff/equal here. To fix this,
         we match for Signal within Cutoff above. *)
      s
  | Both (t1, t2) ->
      React.S.bind ~eq:equal (eval t1) (fun x1 ->
        React.S.map ~eq:phys_equal (fun x2 ->
          (x1, x2)
        ) (eval t2)
      )
  | Map { t; f } ->
      React.S.map ~eq:equal f (eval t)
  | Map2 { t1; t2; f } ->
      React.S.bind ~eq:equal (eval t1) (fun x1 ->
        React.S.map ~eq:phys_equal (fun x2 ->
          f x1 x2
        ) (eval t2)
      )
;;

let eval value = eval value
