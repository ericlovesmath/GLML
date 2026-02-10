(** Generates new symbol starting with argument *)
val fresh : string -> string

(** Resets internal counter used to generate [fresh] variables *)
val reset : unit -> unit
