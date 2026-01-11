type _ t =
  | Constant : 'a -> 'a t
  | Signal : 'a React.signal -> 'a t
  | Cutoff : { a : 'a t; equal : 'a -> 'a -> bool } -> 'a t
  | Map : { a : 'a t; f : 'a -> 'b } -> 'b t
  | Map2 : { a : 'a t; b : 'b t; f : 'a -> 'b -> 'c } -> 'c t

let constant x = Constant x
let signal s = Signal s
let cutoff a ~equal = Cutoff { equal; a }
let both a b = Map2 { b; a; f = (fun a b -> (a, b)) }
let map a ~f = Map { a; f }
let map2 a b ~f = Map2 { a; b; f }
let phys_equal a b = a == b

let rec eval : type a. ?equal:(a -> a -> bool) -> a t -> a React.signal =
 fun ?(equal = phys_equal) value ->
  match value with
  | Cutoff { a = Signal s; equal } ->
      (* We want to apply a cutoff to an existing signal. This requires a
         new signal. *)
      React.S.map ~eq:equal Fun.id s
  | Cutoff { a; equal } ->
      (* Apply the custom cutoff/equal on the next recursive eval step. *)
      eval ~equal a
  | Constant x ->
      (* We have no way to apply the requested cutoff/equal here but as the
         signal is constant we don't have to. *)
      React.S.const x
  | Signal s ->
      (* There's no way to apply the requested cutoff/equal here. To fix this,
         we match for Signal within Cutoff above. *)
      s
  | Map { a; f } -> React.S.l1 ~eq:equal f (eval a)
  | Map2 { a; b; f } -> React.S.l2 ~eq:equal f (eval a) (eval b)

let eval value = eval value (* erase optional equal argument *)
