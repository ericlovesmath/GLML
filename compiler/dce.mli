(** Dead code elimination.

    Removes pure [Let] bindings whose name is unused in the continuation,
    [Set] assignments whose target is not live afterwards (computed by a
    backwards liveness fixed-point that correctly handles [While] loops),
    and top-level [Define]/[Const] declarations not reachable from [main]
    through the call graph. *)
val rewrite : Remove_placeholder.t -> Remove_placeholder.t
