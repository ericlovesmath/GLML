(** Constant folding and propagation, eliminates dead arms when cond is known *)
val rewrite : Anf.t -> Anf.t
