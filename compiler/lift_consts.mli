(** Replaces toplevel constants that can't be translated directly
    to GLSL constant terms into zero-argument functions, along with
    their callsites *)
val lift : Remove_placeholder.t -> Remove_placeholder.t
