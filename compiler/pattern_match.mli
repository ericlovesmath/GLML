(** Maranget pattern-matrix algorithm: http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

    Exhaustiveness / Redundancy checks (for [Typecheck]) and provides
    decision-tree compilation primitives (for [Lower_variants]) *)

open Frontend

(** Finds Missing-pattern witness if the arms do not cover the scrutinee type *)
val is_exhaustive : scrutinee_ty:Type_system.ty -> pat list -> pat option

(** Finds index of the first redundant arm *)
val is_redundant : scrutinee_ty:Type_system.ty -> pat list -> int option

(** Pattern-matrix primitives. Rows pair a column list with
    [unit] for usefulness checking or a [match-arm RHS] for lowering). *)
module Matrix : sig
  type 'a row = pat list * 'a

  (** [`Empty]: no rows - match failure.
      [`Leaf row]: every column of the first row is wild/var; the row's body
                   is what executes (callers bind any [PatVar]s to the current occurrences).
      [`Pivot col]: dispatch on column [col] of the first row. *)
  val classify : 'a row list -> [ `Empty | `Leaf of 'a row | `Pivot of int ]

  (** [S(P)]: wild/var rows expand to [arity] wildcards (body rewritten via
      [on_wild_head]); non-wild rows whose head [expand] rejects are dropped. *)
  val specialize
    :  on_wild_head:(pat -> 'a -> 'a)
    -> expand:(pat -> pat list option)
    -> arity:int
    -> 'a row list
    -> 'a row list

  (** [D(P)]: keep wild/var rows (body rewritten via [on_wild_head]), drop the
      leading column, drop everything else. *)
  val default : on_wild_head:(pat -> 'a -> 'a) -> 'a row list -> 'a row list
end
