(* TODO: Bonsai.Cont uses some Trampoline.t to avoid stack overflows when this
   function is applied. *)
type graph = { mutable f : 'a. 'a Computation.t -> 'a Computation.t }
