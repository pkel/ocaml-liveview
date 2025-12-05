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

val eval : 'a t -> 'a React.signal
