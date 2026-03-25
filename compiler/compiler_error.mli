open Core

type error =
  { pass : string
  ; loc : Lexer.loc option
  ; msg : string
  ; details : Sexp.t option
  }
[@@deriving sexp_of]

type 'a t = ('a, error) Result.t

include Monad.S with type 'a t := 'a t

val fail : pass:string -> ?loc:Lexer.loc -> ?d:Sexp.t -> string -> 'a t
val of_option : pass:string -> ?loc:Lexer.loc -> ?d:Sexp.t -> string -> 'a option -> 'a t
val of_or_error : pass:string -> ?loc:Lexer.loc -> 'a Or_error.t -> 'a t
val to_or_error : 'a t -> 'a Or_error.t
val ok_exn : 'a t -> 'a

(** Per-pass functor to avoid repeating the pass name *)
module Pass (_ : sig
    val name : string
  end) : sig
  val fail : ?loc:Lexer.loc -> ?d:Sexp.t -> string -> 'a t
  val of_option : ?loc:Lexer.loc -> ?d:Sexp.t -> string -> 'a option -> 'a t
  val of_or_error : ?loc:Lexer.loc -> 'a Or_error.t -> 'a t
end
