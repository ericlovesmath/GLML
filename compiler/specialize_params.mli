val mangle_ty : Typecheck.ty -> string
val is_concrete : Typecheck.ty -> bool

(** TODO: this pass is jank and maybe should be in
   [monomorphize], I just didn't want to fold over 5 maps *)
val specialize : Typecheck.t -> Typecheck.t
