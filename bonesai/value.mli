type 'a t

val constant : 'a -> 'a t
val signal : 'a React.signal -> 'a t
val cutoff : 'a t -> equal:('a -> 'a -> bool) -> 'a t
val both : 'a t -> 'b t -> ('a * 'b) t
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

type proof
(** proof that value has been compiled *)

val eval : 'a t -> 'a React.signal * proof
val id : proof -> (string, [ `Outside_computation ]) result
