(** Walk a typed term, rewriting [Coerce] nodes, exposed for
    [Monomorphize] which re-runs after specialization *)
val rewrite : Typecheck.term -> Typecheck.term

(** Materializes [Coerce] constraints in the typed AST. After [Typecheck], the
    solver may have discharged subtype coercions (notably [int <: float]) without
    updating the AST: a child term's [.ty] can disagree with its parent's expectation.

    Goal is that after this pass, every term's [.ty] is the truth without coercions *)
val materialize : Typecheck.t -> Typecheck.t
