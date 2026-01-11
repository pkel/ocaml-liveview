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
let current_node : int option ref = ref None
(* using this global is kind of ugly, but I don't see another way now *)

type context = int ref

let id1 ctx f a =
  let i = !ctx in
  incr ctx;
  current_node := Some i;
  let r = f a in
  current_node := None;
  r

let id2 ctx f a b =
  let i = !ctx in
  incr ctx;
  current_node := Some i;
  let r = f a b in
  current_node := None;
  r

(* use context to count the number of pure computations allocated in the graph;
   then use it to generate the unique identifiers that can be accessed from
   within the computation itself. *)

let rec eval : type a.
    ?equal:(a -> a -> bool) -> context -> a t -> a React.signal =
 fun ?(equal = phys_equal) ctx value ->
  match value with
  | Cutoff { a = Signal s; equal } ->
      (* We want to apply a cutoff to an existing signal. This requires a
         new signal. *)
      React.S.map ~eq:equal Fun.id s
  | Cutoff { a; equal } ->
      (* Apply the custom cutoff/equal on the next recursive eval step. *)
      eval ~equal ctx a
  | Constant x ->
      (* We have no way to apply the requested cutoff/equal here but as the
         signal is constant we don't have to. *)
      React.S.const x
  | Signal s ->
      (* There's no way to apply the requested cutoff/equal here. To fix this,
         we match for Signal within Cutoff above. *)
      s
  | Map { a; f } -> React.S.l1 ~eq:equal (id1 ctx f) (eval ctx a)
  | Map2 { a; b; f } ->
      React.S.l2 ~eq:equal (id2 ctx f) (eval ctx a) (eval ctx b)

type proof = unit

let eval value = (eval (ref 0) value, ())

let id () =
  match !current_node with
  | None -> Error `Outside_computation
  | Some i -> Ok (Printf.sprintf "%x" i)
