type t =
    { loc_start: Lexing.position;
      loc_end:   Lexing.position;
      loc_ghost: bool }

val none: t

val symbol_gloc: unit -> t
val symbol_rloc: unit -> t
val rhs_loc: int -> t
val curr: Lexing.lexbuf -> t
val init: Lexing.lexbuf -> string -> unit

val print: Format.formatter -> t -> unit
val print_error: Format.formatter -> t -> unit
