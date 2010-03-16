type t =
    { loc_start: Lexing.position;
      loc_end:   Lexing.position;
      loc_ghost: bool }

val none: t

val symbol_gloc: unit -> t
val symbol_rloc: unit -> t
val rhs_loc: int -> t
