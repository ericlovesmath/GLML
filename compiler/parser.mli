(** Parser for Stlc.t *)
val glml_p : Stlc.t Chomp.t

val parse : (Lexer.token * Lexer.loc) list -> Stlc.t Compiler_error.t
