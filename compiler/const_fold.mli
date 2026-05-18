(** Constant folding and propagation, eliminates dead arms when cond is known *)
val rewrite : Remove_placeholder.t -> Remove_placeholder.t
