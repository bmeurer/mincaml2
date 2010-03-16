type t =
    { loc_start: Lexing.position;
      loc_end:   Lexing.position;
      loc_ghost: bool }

let none =
  { loc_start = Lexing.dummy_pos;
    loc_end   = Lexing.dummy_pos;
    loc_ghost = true }

let symbol_gloc (_:unit): t =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = true }

let symbol_rloc (_:unit): t =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = false }

let rhs_loc (n:int): t =
  { loc_start = Parsing.rhs_start_pos n;
    loc_end = Parsing.rhs_end_pos n;
    loc_ghost = false }

