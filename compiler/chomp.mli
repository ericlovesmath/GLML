(** Simple parser combinator library *)

open Core
open Lexer

type stream
type 'a maybe
type 'a t = stream -> ('a * stream) maybe

(** Run parser [p] on string [s], ensuring the entire input is consumed.
    Error messages state line/col of error *)
val run : 'a t -> (token * loc) list -> 'a Or_error.t

include Applicative.S with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

module Infix_syntax : sig
  include Applicative.Applicative_infix with type 'a t := 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <$ ) : 'a -> 'b t -> 'a t
  val ( $> ) : 'a t -> 'a -> 'a t

  (* NOTE: [Applicative_infix] requires usage of [ignore_m], which is cumbersome.
    This replaces [*>] and [<*] to use Haskell-like syntax, implicitly ignoring *)
  val ( *> ) : 'ignore t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'ignore t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <*>| ) : ('a -> 'b) t -> 'a t Lazy.t -> 'b t

  (** Annotate with error message *)
  val ( <??> ) : 'a t -> string -> 'a t
end

(** If commited, alternatives don't backtrack *)
val commit : 'a t -> 'a t

val satisfy : (token -> bool) -> token t
val satisfy_map : (token -> 'a option) -> 'a t
val peek : token t
val many1 : 'a t -> 'a list t
val many : 'a t -> 'a list t
val sep_by1 : 'a t -> 'b t -> 'b list t
val sep_by : 'a t -> 'b t -> 'b list t
val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
val optional : 'a t -> 'a option t
val tok : token -> token t
val fail : string -> 'a t
val fatal : string -> 'a t

(** Records the start position before running [p] and the end position after running [p]. *)
val with_loc : 'a t -> ('a * loc) t
