(** Dead code elimination.

    Removes unused [Let] bindings and declarations not reachable from [main] *)
val rewrite : Anf.t -> Anf.t
