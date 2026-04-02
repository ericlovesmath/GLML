type env = Typecheck.type_decl Core.String.Map.t

val mangle_ty : Typecheck.ty -> string
val is_concrete : Typecheck.ty -> bool
val rewrite_term : env:env -> Typecheck.term -> Typecheck.term
val rewrite_ty : env:env -> Typecheck.ty -> Typecheck.ty

(** TODO: this pass is jank and maybe should be in
   [monomorphize], I just didn't want to fold over 5 maps *)
val specialize : Typecheck.t -> Typecheck.t * env
